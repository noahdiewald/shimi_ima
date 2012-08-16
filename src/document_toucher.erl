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
         touch_all/2,
         touch_all/3
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("config.hrl").
-include_lib("types.hrl").

-record(state {
          wrq :: utils:reqdata(),
          wm_state :: any(),
          tid :: ets:tid(),
          doctype :: string()
          }).

-spec touch_all(utils:reqdata(), any()) -> -> {ok, pid()} | ignore | {error, term()}.
start(R, WMS) ->
    touch_all(wrq:path_info(id, R), R, WMS).

-spec touch_all(string(), utils:reqdata(), any()) -> {ok, pid()} | ignore | {error, term()}.
start(Id, R, WMS) ->
    gen_server:start({local, me(Id)}, ?MODULE, {Id, R, WMS}, []).

run(Pid) ->
    gen_server:send(Pid, initialize).

me(Doctype) ->
    list_to_atom(atom_from_list(?SERVER) ++ "-" ++ Doctype).

my_table(Doctype) ->
    list_to_atom(Doctype ++ "-" ++ "fs_table").

init({Id, R, WMS}) ->
    error_logger:info_report([{touch_all, started}]),
    FSTid = ets:new(list_to_atom(Doctype ++ "-" ++ "fs_table"), []),
    FTid = ets:new(list_to_atom(Doctype ++ "-" ++ "f_table"), []),
    {ok, #state{doctype=Id, wrq=R, wm_state=WMS, fstid=FSTid, ftid=FTid}}.

handle_call(_Msg, _From, S) ->    
    {noreply, S}.

handle_info(initialize, S) ->
    error_logger:info_report([{touch_all, initializing}]),
    erlang:send(me(S#state.doctype), fill_tab),
    {noreply, S};
handle_info({fill_tab, []}, S) ->
    erlang:send(me(S#state.doctype), get_docs),
    {noreply, S};
handle_info({fill_tab, [H|T]}, 
            S=#state{doctype=Doctype,wrq=R,wm_state=WMS}) ->
    case couch:get_view_json(H, "fields", R, WMS) of
        {ok, FJson} ->
            FRecs = [{jsn:get_value(<<"id">>, X), 
                      field:from_json(jsn:get_value(<<"value">>, X))} || 
                        X <- jsn:get_value(<<"rows">>, FJson)],
            Tid = ets:new(atom_to_list(H), [named_table]),
            true = ets:insert(Tid, FRecs),
            erlang:send(me(Doctype), {fill_tab, T});
        {error, req_timedout} ->
            erlang:send_after(150000, me(Doctype), {fill_tab, [H|T]})
    end,
    {noreply, S};
handle_info(fill_tab, S=#state{doctype=Doctype,wrq=R,wm_state=WMS,tid=Tid}) ->
    case couch:get_view_json(Doctype, "fieldsets", R, WMS) of
        {ok, FSJson} ->
            F = fun(X, {Ids, Recs}) ->
                        Id = jsn:get_value(<<"id">>, X),
                        Id2 = binary_to_list(Id),
                        Rec = fieldset:from_json(jsn:get_value(<<"value">>, X)),
                        {[Id2|Ids], [{Id, Rec}|Recs]}
                end,
            {FSIds, FSRecs} = lists:foldl(F, {[], []}, 
                                          jsn:get_value(<<"rows">>, FSJson)),
            true = ets:insert(Tid, FSRecs),
            erlang:send(me(Doctype), {fill_tab, FSIds});
        {error, req_timedout} ->
            erlang:send_after(150000, me(Doctype), fill_tab)
    end,
    {noreply, S};
handle_info(get_docs, S=#state{doctype=Doctype, wrq=R, wm_state=WMS}) ->
    Docs = case untouched:start(Doctype, []) of
               {error, already_started} -> 
                   untouched:reload(Id);
               {error, no_documents} -> 
                   case couch:get_view_json(Doctype, "quickdocs", R, WMS) of
                       {ok, AllDocs} ->
                           Rows = jsn:get_value(<<"rows">>, AllDocs),
                           {ok, _} = untouched:start(Doctype, Rows),
                           Rows;
                       {error, req_timedout} ->
                           erlang:send_after(150000, me(Doctype), get_docs)
                   end,
           end,
    F = fun (Row) -> touch(jsn:get_value(<<"id">>, Row), S) end,
    spawn(?MODULE, peach, [F, Docs, 5]),
    {noreply, S};
handle_info(_Msg, S) ->
    {noreply, S}.
  
terminate(_Msg, S) ->
    error_logger:info_report([{touch_all, finished}]),
    ok.
    
-spec touch(binary(), utils:reqdata(), any()) -> ok.
touch(Id, S) ->
    Json = touch_get_json(Id, S#state.wrq, S#state.wm_state),
    Doc = document:from_json(Json),
    Doc2 = document:to_json(
             Doc#document{
               prev = Doc#document.rev,
               fieldsets = fieldset_touch_all(Doc#document.fieldsets, S)}),
    case couch:update(doc, binary_to_list(Id), jsn:encode(Doc2), R, S) of
        {ok, updated} -> ok;
        Unexpected ->
            error_logger:error_report(
              [{touch_document_update, {Unexpected, Doc2}}])
    end.

-spec touch_get_json(binary(), utils:reqdata(), any()) -> json:json_term().
touch_get_json(Id, R, S) ->                
    {ok, Json} = couch:get_json(safer, binary_to_list(Id), R, S),
    Json.

%% @doc Take a function a list and a number and run the function over
%% list items in parallel. The number limits the number of concurrent
%% workers. The operation is run as a side effect, returning the atom ok.
-spec peach(F :: fun(), L :: list(), N :: integer()) -> ok.
peach(_F, [], _N) ->
    ok;
peach(F, L, N) ->
    {First, Rest} = takedrop(L, N),
    S = self(),
    Ref = erlang:make_ref(),
    Ref2 = erlang:make_ref(),
    Loop = spawn(fun() -> loop(Ref, {Ref2, S}, F, Rest) end),
    Fun = fun(I) -> spawn(fun() -> do_f({Ref, Loop}, {Ref2, S}, F, I) end) end,
    lists:foreach(Fun, First),
    counter(Ref2, length(L)).

takedrop(L, N) ->
    takedrop(L, [], N).

takedrop([], Acc, _N) ->
    {lists:reverse(Acc), []};
takedrop(Rest, Acc, 0) ->
    {lists:reverse(Acc), Rest};
takedrop([H|Rest], Acc, N) ->
    takedrop(Rest, [H|Acc], N - 1).
    
counter(_, 0) -> ok;
counter(Ref, N) ->
    receive
        Ref ->
            counter(Ref, N - 1)
    end.

do_f({Ref, Parent}, {Ref2, Counter}, F, I) ->
    case (catch F(I)) of
        {'EXIT', Error} ->
            error_logger:error_report(
              [{error_in_peach, {Error, {with_args, I}}}]);
        _ -> ok
    end,
    Parent ! Ref,
    Counter ! Ref2.

loop(_, _, _, []) ->
    ok;
loop(Ref, Counter, F, [Next|Rest]) ->
    S = self(),
    receive
        Ref ->
            spawn(fun() -> do_f({Ref, S}, Counter, F, Next) end),
            loop(Ref, Counter, F, Rest)
    end.
