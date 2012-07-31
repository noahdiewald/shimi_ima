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

-type database() :: string(). % A database name.
-type update_seq() :: integer(). % The current update sequence.

-record(state, {
          db_seqs :: [{database(), update_seq()}]
          }).

-type state() :: #state{}.

-export([
         start_link/1,
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

-spec start_link([{database(), update_seq()}]) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec update_views(database()) -> ok.
update_views(DB) ->
    gen_server:cast(?MODULE, DB),
    ok.

-spec init([{database(), update_seq()}]) -> {ok, state()}.
init(DBSeqs) ->
    S = #state{db_seqs = DBSeqs},
    {ok, S}.

handle_call(_Request, _From, S) ->
    {noreply, S}.

handle_cast(DB, S) ->
    io:format("Called!!!"),
    S1 = update_views(DB, S),
    {noreply, S1}.

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

update_views(DB, S=#state{db_seqs = DBSeq}) ->
    io:format("--~p--~p--", [DB, DBSeq]),
    case proplists:get_value(DB, DBSeq) of
        undefined ->
            S1 = find_new_database(DB, S),
            update_views(DB, S1);
        OldSeq ->
            update_views(DB, OldSeq, S)
    end.
    
update_views(DB, OldSeq, S) ->
    case couch:get_db_seq(DB) of
        undefined ->
            #state{db_seqs = proplists:delete(DB, S#state.db_seqs)};
        {ok, NewSeq} -> 
            update_views(DB, OldSeq, NewSeq, S)
    end.

update_views(DB, OldSeq, NewSeq, S) when (NewSeq - OldSeq) > 10 ->
    Views = utils:shuffle(couch:get_views(DB)),
    lists:map(fun (X) -> update_view(DB, X) end, Views),
    Seq = proplists:delete(DB, S#state.db_seqs),
    #state{db_seqs = [{DB, NewSeq}|Seq]};
update_views(_, _, _, S) ->
    S.

update_view(DB, [Path]) ->
    case couch:get_silent(DB, binary_to_list(Path)) of
        ok -> ok;
        {error, req_timedout} -> update_view(DB, Path)
    end.

find_new_database(DB, S) ->
    {ok, Seq} = couch:get_db_seq(DB),
    #state{db_seqs = [{DB, Seq}|S#state.db_seqs]}.
