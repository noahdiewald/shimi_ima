%%% Copyright 2012 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of Ʃimi Ima.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% dictionary_maker is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General
%%% Public License along with dictionary_maker. If not, see
%%% <http://www.gnu.org/licenses/>.

%%% @copyright 2012 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc Keep track of database update sequences.

-module(database_seqs).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([
         delete_seq/1,
         get_all/0,
         get_seq/1,
         set_all/1,
         set_seq/2,
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

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

delete_seq(DB) ->
    gen_server:call(?SERVER, {delete, DB}).

get_all() ->
    gen_server:call(?SERVER, get_all).

get_seq(DB) ->
    gen_server:call(?SERVER, {get, DB}).

set_all(DBSeqs) ->
    gen_server:call(?SERVER, {set_all, DBSeqs}).

set_seq(DB, Seq) ->
    gen_server:call(?SERVER, {set, DB, Seq}).

init([]) ->
    erlang:send_after(5000, ?SERVER, initialize),
    {ok, dict:new()}.

handle_call({delete, DB}, _From, S) ->
    S1 = dict:erase(DB, S),
    {reply, S1, S1};
handle_call({get, DB}, _From, S) ->
    case dict:find(DB, S) of
        {ok, Val} -> {reply, Val, S};
        error -> {reply, undefined, S}
    end;
handle_call({set, DB, Seq}, _From, S) ->
    S1 = dict:store(DB, Seq, S),
    {reply, S1, S1};
handle_call(get_all, _From, S) ->
    {reply, S, S};
handle_call({set_all, S1}, _From, _S) ->
    {reply, S1, S1};
handle_call(_Msg, _From, S) ->
    {noreply, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(initialize, _S) ->
    Json = project:all(),
    DBSeqs = make_seqs(Json),
    {noreply, DBSeqs}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

make_seqs(Json) ->
    Rows = jsn:get_value(<<"rows">>, Json),
    DBs = get_all_dbs(Rows, []),
    set_seqs(DBs, dict:new()).

get_all_dbs([], Acc) ->
    Acc;
get_all_dbs([H|T], Acc) ->
    IdBin = jsn:get_value(<<"id">>, H),
    Id = binary_to_list(IdBin),
    get_all_dbs(T, [Id|Acc]).

set_seqs([], Acc) ->
    Acc;
set_seqs([H|T], Acc) ->
    case couch:get_db_seq_num(H) of
        {error, not_found} -> set_seqs(T, Acc);
        {ok, Seq} -> set_seqs(T, dict:store(H, Seq, Acc))
    end.
