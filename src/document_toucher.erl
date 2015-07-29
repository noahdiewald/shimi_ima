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
%%% @doc This module contains functions for touching documents. In
%%% this context "touching" refers to a maintenance action where all
%%% documents are updated and made to conform to any changes in
%%% fieldsets, fields, charseqs, etc.

-module(document_toucher).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([
         exists/1,
         start/3,
         stop/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-include_lib("types.hrl").

-record(state, {
          project :: string(),
          wm_state :: any(),
          tid :: ets:tid(),
          doctype :: string(),
          counter :: integer(),
          me :: atom()
          }).

-type state() :: #state{}.

% API

-spec start(string(), string(), any()) -> {ok, pid()} | ignore | {error, term()}.
start(Doctype, Project, WMS) ->
    case gen_server:start({local, me(Doctype)}, ?MODULE, {Doctype, Project, WMS}, [])
    of
        {ok, Pid} -> 
            run(Doctype),
            {ok, Pid};
        Else -> Else
    end.

stop(Doctype) ->
    gen_server:cast(me(Doctype), stop).

exists(Doctype) ->
    lists:member(me(Doctype), registered()).
    
% Gen Server

init({Doctype, Project, WMS}) ->
    error_logger:info_msg("~p started~n", [me(Doctype)]),
    Tid = ets:new(list_to_atom(Doctype ++ "-" ++ "fs_table"), []),
    {ok, #state{doctype=Doctype, 
                project=Project,
                wm_state=WMS, 
                tid=Tid, 
                counter=5, 
                me=me(Doctype)}}.

handle_call(_Msg, _From, S) ->    
    {noreply, S}.

handle_cast(initialize, S) ->
    Me = S#state.me,
    error_logger:info_msg("~p initializing~n", [Me]),
    erlang:send(Me, fill_tab),
    {noreply, S};
handle_cast(stop, S) ->
    {stop, normal, S};
handle_cast(process_docs, S) ->
    case process_docs(S) of
        false ->
            erlang:send_after(150000, S#state.me, get_docs); 
        true ->
            gen_server:cast(S#state.me, stop)
    end,
    {noreply, S};
handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(fill_tab, S) ->
    case fill_tables(S) of
        ok ->
            erlang:send(S#state.me, get_docs);
        {error, req_timedout} ->
            erlang:send_after(150000, S#state.me, fill_tab)
    end,
    {noreply, S};
handle_info(get_docs, S) ->
    case get_docs(S) of
        ok -> 
            gen_server:cast(S#state.me, process_docs);
        {error, req_timedout} ->
            erlang:send_after(150000, S#state.me, get_docs)
    end,
    {noreply, S};
handle_info(_Msg, S) ->
    {noreply, S}.
  
terminate(_Msg, S) ->
    error_logger:info_msg("Stoping ~p~n", [S#state.me]),
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

% Private

%% @doc After initialization is complete, begin the touch operation.
-spec run(string()) -> ok.
run(Doctype) ->
    gen_server:cast(me(Doctype), initialize).

%% @doc Use a doctype argument to retreive the current server's name.
-spec me(string()) -> atom().
me(Doctype) ->
    list_to_atom(atom_to_list(?SERVER) ++ "-" ++ Doctype).
    
%% @doc Touch a document. This means that it will be made to conform
%% to the current state of its doctype's configuration.
-spec touch_document(string(), state()) -> ok.
touch_document(Id, S) ->
    Project = S#state.project,
    WMS = S#state.wm_state,
    {ok, Json} = h:get(binary_to_list(Id), Project, WMS),
    Json1 = document:set_sortkeys(Json, Project, S),
    NormJson = document:normalize(doc, Json1),

    case couch:update(binary_to_list(Id), NormJson, Project, WMS) of
        {ok, _} ->
            untouched:delete(S#state.doctype, Id),
            ok;
        {error, conflict} ->
            error_logger:error_report(
              [{S#state.me, {conflict, NormJson}}]);
        {error, req_timedout} ->
            error_logger:error_report(
              [{S#state.me, {req_timedout, NormJson}}])
    end.

%% @doc Retrieve the ids of the documents that need to be
%% processed. These are kept in the untouched server so that the
%% process can be restarted if there is a network error.
-spec get_docs(state()) -> ok | {error, req_timedout}.
get_docs(#state{doctype=Doctype, project=Project, wm_state=WMS}) ->
    case untouched:exists(Doctype) of
        true -> ok;
        false -> 
            case q:index(Doctype, [], Project, WMS) of
                {ok, AllDocs} ->
                    Rows = jsn:get_value(<<"rows">>, AllDocs),
                    {ok, _} = untouched:start(Doctype, Rows),
                    ok;
                {error, req_timedout} ->
                    {error, req_timedout}
            end
    end.

%% @doc Run touch on each document that is still untouched.
-spec process_docs(state()) -> boolean().
process_docs(S) ->
    case untouched:get(S#state.doctype) of
        [] -> 
            untouched:delete(S#state.doctype, ""),
            not untouched:exists(S#state.doctype);
        DocIds ->
            error_logger:info_msg("~p will touch ~p documents~n", 
                                  [S#state.me, length(DocIds)]),
            F = fun(X) -> touch_document(X, S) end,
            utils:peach(F, DocIds, S#state.counter),
            not untouched:exists(S#state.doctype)
    end.

%% @doc Fill ets tables that hold the fieldsets and fields so that
%% they can be referred to later without adding to server load. The
%% means of retrieving the fieldsets and fields is to use a view and
%% querying the view while updating many documents can cause
%% processing to be slowed while view indexing takes place.
-spec fill_tables(state()) -> {ok, [string()]} | {error, req_timedout}.
fill_tables(#state{doctype=Doctype, project=Project, wm_state=WMS, tid=Tid}) ->
    case q:fieldset(Doctype, Project, WMS) of
        {ok, Json} ->
            ok = fill_tables(jsn:get_value(<<"rows">>, Json), Tid);
        {error, req_timedout} ->
            {error, req_timedout}
    end.

-spec fill_tables(jsn:json_term(), ets:tid()) -> ok.
fill_tables([], _Tid) ->
    ok;
fill_tables([H|T], Tid) ->
    [_, FSID, Type, _] = jsn:get_value(<<"key">>, H),
    FieldTable = list_to_atom(binary_to_list(FSID)),
    case Type of
        <<"fieldset">> ->
            Rec = fieldset:from_json(jsn:get_value(<<"doc">>, H)),
            true = ets:insert(Tid, [{FSID, Rec}]),
            ets:new(FieldTable, [named_table]);
        <<"fieldset-field">> ->
            FID = jsn:get_value(<<"id">>, H),
            Rec = field:from_json(jsn:get_value(<<"doc">>, H)),
            true = ets:insert(FieldTable, [{FID, Rec}])
    end,
    fill_tables(T, Tid).
