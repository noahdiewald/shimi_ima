%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of dictionary_maker.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% dictionary_maker is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with dictionary_maker. If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2011 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc Loop through the views continuously making get requests so
%%% that users aren't surprised by excessive lag time.

-module(view_updater).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(FREQ, 5000).

-record(state, {
          db :: string(),
          views :: [binary()],
          server :: atom(),
          start_seq :: integer()
         }).

-export([
         update_views/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

-spec update_views(string()) -> ok.
update_views(DB) ->
    Server = list_to_atom(atom_to_list(?SERVER) ++ DB),
    case lists:member(Server, registered()) of
        false ->
            Views = utils:shuffle(couch:get_views(DB)),
            StartSeq = database_seqs:get_seq(DB),
            InitState = #state{db = DB,
                               views = Views,
                               server = Server,
                               start_seq = StartSeq},
            gen_server:start({local, Server}, ?MODULE, InitState, []);
        true -> already_started
    end.

-spec init({string(), [binary()]}) -> {ok, {string(), [binary()]}}.
init(S=#state{server=Server}) ->
    erlang:send(Server, trigger),
    {ok, S}.

handle_call(_Request, _From, S) ->
    {noreply, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(trigger, S=#state{db=DB, views=[], start_seq=StartSeq}) ->
    case couch:get_db_seq(DB) of
        undefined -> database_seqs:delete_seq(DB);
        {error, _} -> ok;
        {ok, EndSeq} ->
            % If the difference is great enough, write load may be
            % high enough to take another pass.
            database_seqs:set_seq(DB, (EndSeq + StartSeq) div 2)
    end,
    {stop, normal, S};
handle_info(trigger, S=#state{db=DB, views=Views, server=Server}) ->
    erlang:send(Server, {DB, Views}),
    {noreply, S#state{views=[]}};
handle_info({_DB, []}, S) ->
    erlang:send(S#state.server, trigger),
    {noreply, S};
handle_info({DB, [Path|Rest]}, S) ->
    LR = length(Rest),
    if 
        (LR rem 10) == 0 ->
            error_logger:info_msg("View updating ~p: ~p views left~n", 
                                  [DB, LR]);
         true -> ok
    end,
    case update_view(DB, Path) of
        true -> erlang:send(S#state.server, {DB, Rest});
        false -> erlang:send_after(150000, S#state.server, {DB, Rest})
    end,
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

update_view(DB, Path) ->
    not couch:should_wait(DB, binary_to_list(Path)).
