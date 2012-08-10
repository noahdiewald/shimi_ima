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

-module(database_monitor).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([
         start_link/0
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    DBSeqs = database_seqs:get_all(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, DBSeqs, []).

-spec init(dict()) -> {ok, dict()}.
init(DBSeqs) ->
    erlang:send(?SERVER, {trigger, DBSeqs}),
    {ok, []}.

handle_call(_Request, _From, S) ->
    {noreply, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({trigger, DBSeqs}, S) ->
    {DBs, DBSeqs1} = get_ready_dbs(DBSeqs),
    erlang:send(?SERVER, {DBs, DBSeqs1}),
    {noreply, S};
handle_info(refresh, S) ->
    DBSeqs = database_seqs:get_all(),
    erlang:send(?SERVER, {trigger, DBSeqs}),
    {noreply, S};
handle_info({[], _}, S) ->
    erlang:send_after(30000, ?SERVER, refresh),
    {noreply, S};
handle_info({[{DB, _}|Rest], DBSeqs}, S) ->
    view_updater:update_views(DB),
    erlang:send(?SERVER, {Rest, DBSeqs}),
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

get_ready_dbs(DBSeqs) ->
    Pred = fun(DB, OldSeq) ->
                   case couch:get_db_seq(DB) of
                       undefined -> false;
                       {ok, NewSeq} -> (NewSeq - OldSeq) > 10
                   end
           end,
    Merger = fun(_, V, _) -> V end,
    Ready = dict:filter(Pred, DBSeqs),
    {dict:to_list(Ready), dict:merge(Merger, Ready, DBSeqs)}.
