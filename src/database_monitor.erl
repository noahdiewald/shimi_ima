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
    Json = couch:get_dbs(),
    DBSeqs = make_seqs(Json), 
    gen_server:start_link({local, ?SERVER}, ?MODULE, DBSeqs, []).

-spec init(dict()) -> {ok, dict()}.
init(DBSeqs) ->
    erlang:send_after(300000, ?MODULE, trigger),
    {ok, DBSeqs}.

handle_call(_Request, _From, S) ->
  {noreply, S}.

handle_cast(_Msg, S) ->
  {noreply, S}.

handle_info(trigger, S) ->
  {DBs, S1} = get_ready_dbs(S),
  erlang:send(?MODULE, DBs),
  {noreply, S1};
handle_info([], S) ->
  erlang:send_after(300000, ?MODULE, trigger),
  {noreply, S};
handle_info([{DB,_}|Rest], S) ->
  view_updater:update_views(DB),
  erlang:send_after(300000, ?MODULE, Rest),
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

make_seqs(Json) ->
    Rows = jsn:get_value(<<"rows">>, Json),
    DBs = get_all_dbs(Rows, []),
    set_seqs(DBs, dict:new()).

get_all_dbs([], Acc) ->
    Acc;
get_all_dbs([H|T], Acc) ->
    Id = binary_to_list(jsn:get_value(<<"id">>, H)),
    get_all_dbs(T, ["project-" ++ Id, "files-" ++ Id|Acc]).

set_seqs([], Acc) ->
    Acc;
set_seqs([H|T], Acc) ->
    case couch:get_db_seq(H) of
        undefined -> set_seqs(T, Acc);
        {ok, Seq} -> set_seqs(T, dict:store(H, Seq, Acc))
    end.
