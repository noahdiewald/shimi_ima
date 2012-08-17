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
         start/2,
         start/3
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("config.hrl").
-include_lib("types.hrl").

-record(state, {
          wrq :: utils:reqdata(),
          wm_state :: any(),
          tid :: ets:tid(),
          doctype :: string(),
          counter :: integer()
          }).

-type state() :: #state{}.

% API

-spec start(utils:reqdata(), any()) -> {ok, pid()} | ignore | {error, term()}.
start(R, WMS) ->
    start(wrq:path_info(id, R), R, WMS).

-spec start(string(), utils:reqdata(), any()) -> {ok, pid()} | ignore | {error, term()}.
start(Doctype, R, WMS) ->
    case gen_server:start({local, me(Doctype)}, ?MODULE, {Doctype, R, WMS}, [])
    of
        {ok, Pid} -> 
            run(Doctype),
            {ok, Pid};
        Else -> Else
    end.

% Gen Server

init({Doctype, R, WMS}) ->
    error_logger:info_report([{touch_all, started}]),
    Tid = ets:new(list_to_atom(Doctype ++ "-" ++ "fs_table"), []),
    {ok, #state{doctype=Doctype, wrq=R, wm_state=WMS, tid=Tid, counter=5}}.

handle_call(_Msg, _From, S) ->    
    {noreply, S}.

handle_cast(initialize, S) ->
    error_logger:info_report([{touch_all, initializing}]),
    erlang:send(me(S#state.doctype), fill_tab),
    {noreply, S};
handle_cast({document, Id}, S) when is_binary(Id) ->
    touch_document(Id, S),
    {noreply, S};
handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({fill_tab, FS}, S) ->
    fill_field_tables(FS, S),
    {noreply, S};
handle_info(fill_tab, S) ->
    fill_fieldset_table(S),
    {noreply, S};
handle_info(get_docs, S) ->
    get_docs(S),
    %process_docs(S),
    {noreply, S};
handle_info(_Msg, S) ->
    {noreply, S}.
  
terminate(_Msg, _S) ->
    error_logger:info_report([{touch_all, finished}]),
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

% Private
    
%% -spec touch(binary(), utils:reqdata(), any()) -> ok.
%% touch(Id, S) ->
%%     Json = touch_get_json(Id, S#state.wrq, S#state.wm_state),
%%     Doc = document:from_json(Json),
%%     Doc2 = document:to_json(
%%              Doc#document{
%%                prev = Doc#document.rev,
%%                fieldsets = fieldset_touch_all(Doc#document.fieldsets, S)}),
%%     case couch:update(doc, binary_to_list(Id), jsn:encode(Doc2), R, S) of
%%         {ok, updated} -> ok;
%%         Unexpected ->
%%             error_logger:error_report(
%%               [{touch_document_update, {Unexpected, Doc2}}])
%%     end.

-spec touch_get_json(binary(), utils:reqdata(), any()) -> json:json_term().
touch_get_json(Id, R, S) ->                
    {ok, Json} = couch:get_json(safer, binary_to_list(Id), R, S),
    Json.

%% @doc Take a function a list and a number and run the function over
%% list items in parallel. The number limits the number of concurrent
%% workers. The operation is run as a side effect, returning the atom ok.
%% -spec peach(F :: fun(), L :: list(), N :: integer()) -> ok.
%% peach(_F, [], _N) ->
%%     ok;
%% peach(F, L, N) ->
%%     {First, Rest} = lists:split(N, L),
%%     S = self(),
%%     Ref = erlang:make_ref(),
%%     Ref2 = erlang:make_ref(),
%%     Loop = spawn(fun() -> loop(Ref, {Ref2, S}, F, Rest) end),
%%     Fun = fun(I) -> spawn(fun() -> do_f({Ref, Loop}, {Ref2, S}, F, I) end) end,
%%     lists:foreach(Fun, First),
%%     counter(Ref2, length(L)).
    
%% counter(_, 0) -> ok;
%% counter(Ref, N) ->
%%     receive
%%         Ref ->
%%             counter(Ref, N - 1)
%%     end.

%% do_f({Ref, Parent}, {Ref2, Counter}, F, I) ->
%%     case (catch F(I)) of
%%         {'EXIT', Error} ->
%%             error_logger:error_report(
%%               [{error_in_peach, {Error, {with_args, I}}}]);
%%         _ -> ok
%%     end,
%%     Parent ! Ref,
%%     Counter ! Ref2.

%% loop(_, _, _, []) ->
%%     ok;
%% loop(Ref, Counter, F, [Next|Rest]) ->
%%     S = self(),
%%     receive
%%         Ref ->
%%             spawn(fun() -> do_f({Ref, S}, Counter, F, Next) end),
%%             loop(Ref, Counter, F, Rest)
%%     end.

run(Doctype) ->
    gen_server:cast(me(Doctype), initialize).

me(Doctype) ->
    list_to_atom(atom_to_list(?SERVER) ++ "-" ++ Doctype).

-spec touch_fieldsets([docfieldset()], state()) -> [docfieldset()].
touch_fieldsets(FSs, S) ->
    FSs2 = add_missing(FSs, S),
    Fun = fun(X, Acc) ->
                  case touch_fieldset(X, S) of
                      undefined -> Acc;
                      FS -> [FS|Acc]
                  end
          end,
    lists:foldl(Fun, [], FSs2). 

-spec touch_fields([docfield()], atom(), state()) -> [docfield()].
touch_fields(Fs, FTid, S) ->        
    Fs2 = add_missing(Fs, FTid),
    Fun = fun(X, Acc) ->
                  case touch_field(X, FTid, S) of
                      undefined -> Acc;
                      F -> [F|Acc]
                  end
          end,
    lists:foldl(Fun, [], Fs2).
    
-spec touch_fieldset(docfieldset(), state()) -> docfieldset() | undefined.
touch_fieldset(DFS, S) ->
    case ets:lookup(S#state.tid, DFS#docfieldset.id) of
        [] -> undefined;
        [FS] ->
            DFS2 = DFS#docfieldset{
                     label = FS#fieldset.label,
                     name = FS#fieldset.name,
                     order = FS#fieldset.order,
                     multiple = set_if_undefined(
                                  DFS#docfieldset.multiple, 
                                  FS#fieldset.multiple),
                     collapse = FS#fieldset.collapse
                    },
            FTid = list_to_atom(binary_to_list(DFS2#docfieldset.id)),
            Fs = case {DFS2#docfieldset.multiple, DFS2#docfieldset.fields} of
                     {true, undefined} -> [[]];
                     {true, F} -> [touch_fields(X, FTid, S) || X <- F];
                     {false, undefined} -> [];
                     {false, F} -> touch_fields(F, FTid, S)
                 end,
            DFS2#docfieldset{fields=Fs}
    end.

-spec touch_field(docfield(), atom(), state()) -> docfield() | undefined.
touch_field(DF, FTid, S) ->
    case ets:lookup(FTid, DF#docfield.id) of
        [] -> undefined;
        [F] ->
            DF2 = DF#docfield{
                    charseq = F#field.charseq,
                    head = F#field.head,
                    label = F#field.label,
                    max = F#field.max,
                    min = F#field.min,
                    name = F#field.name,
                    order = F#field.order,
                    regex = F#field.regex,
                    required = F#field.required,
                    reversal = F#field.reversal,
                    subcategory = F#field.subcategory
                   },
            DF3 = update_normalize(DF, F, DF2),
            DF3#docfield{
              sortkey=charseq:get_sortkey(DF3, S#state.wrq, S#state.wm_state)}
    end.

set_if_undefined(undefined, Val) ->
    Val;
set_if_undefined(Val, _) ->
    Val.

-spec touch_document(string(), state()) -> ok.
touch_document(Id, S) ->
    Json = touch_get_json(Id, S#state.wrq, S#state.wm_state),
    Doc = document:from_json(Json),
    Fieldsets = touch_fieldsets(Doc#document.fieldsets, S),
    Doc2 = document:to_json(Doc#document{prev = Doc#document.rev,
                                         fieldsets = Fieldsets}),
    case couch:update(doc, binary_to_list(Id), jsn:encode(Doc2), S#state.wrq, 
                      S#state.wm_state) of
        {ok, updated} ->
            untouched:delete(Id),
            ok;
        Unexpected ->
            error_logger:error_report(
              [{touch_document_update, {Unexpected, Doc2}}])
    end.

-spec add_missing([docfieldset()], state() | atom()) -> [docfieldset()] | [docfield()].
add_missing(Fs, FTid) when is_atom(FTid) ->
    Ids = [X#docfield.id || X <- Fs],
    Fun = fun({Id, _}, Acc) ->
                  case lists:member(Id, Ids) of
                      false -> [#docfield{id=Id}|Acc];
                      true -> Acc
                  end
          end,
    ets:foldl(Fun, Fs, FTid);
add_missing(FSs, S) ->
    Ids = [X#docfieldset.id || X <- FSs],
    Fun = fun({Id, _}, Acc) ->
                  case lists:member(Id, Ids) of
                      false -> [#docfieldset{id=Id}|Acc];
                      true -> Acc
                  end
          end,
    ets:foldl(Fun, FSs, S#state.tid).
    
-spec get_docs(state()) -> reference() | ok.
get_docs(#state{doctype=Doctype, wrq=R, wm_state=WMS}) ->
    case untouched:start(Doctype, []) of
        {error, already_started} -> ok;
        {error, no_documents} -> 
            case couch:get_view_json(Doctype, "quickdocs", R, WMS) of
                {ok, AllDocs} ->
                    Rows = jsn:get_value(<<"rows">>, AllDocs),
                    {ok, _} = untouched:start(Doctype, Rows),
                    ok;
                {error, req_timedout} ->
                    erlang:send_after(150000, me(Doctype), get_docs)
            end
    end.

-spec fill_fieldset_table(state()) -> reference() | ok.
fill_fieldset_table(#state{doctype=Doctype,wrq=R,wm_state=WMS,tid=Tid}) ->
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
    end.

-spec fill_field_tables([string()], state()) -> reference() | ok.
fill_field_tables([], S) ->
    erlang:send(me(S#state.doctype), get_docs);
fill_field_tables([H|T], #state{doctype=Doctype, wrq=R, wm_state=WMS}) -> 
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
    end.

%% @doc These are the fairly complex rules that deal with a fields
%% value when default values, categories or other options have
%% changed. The first argument is the previous state of the document
%% field. The second is the field that the docfield is based on and
%% the third argument is the version of the docfield being updated,
%% which is already partially processed.
-spec update_normalize(docfield(), field(), docfield()) -> docfield() | {error, Reason :: term()}.
% if the value of the docfield is the default, leave it.
update_normalize(_, #field{default=X}, DF=#docfield{value=X}) ->
    DF;
% if the value is null, set it to the default.
update_normalize(_, #field{default=Def}, DF=#docfield{value=null}) -> 
    DF#docfield{value=Def};
% this is partially to avoid having undefined be the value. This will
% ensure the default or null.
update_normalize(_, #field{default=Def}, DF=#docfield{subcategory=openboolean,
                                                      value=V}) when
      (V /= true) and (V /= false) -> DF#docfield{value=Def};
%  this is partially to avoid having undefined be the value. If the
%  value isn't a list, then it is unset and the default should be set.
update_normalize(_, #field{default=Def}, DF=#docfield{subcategory=S,value=V}) 
  when
      ((S == multiselect) or (S == docmultiselect)) and (not is_list(V)) -> 
    DF#docfield{value=Def};
% if the subcategory hasen't changed or is changing from a string type
% to text or textarea, leave it alone.
update_normalize(#docfield{subcategory=S}, _, DF=#docfield{subcategory=S2}) 
  when 
    (S == S2) or
    (((S == text) or
      (S == textarea) or
      (S == select) or
      (S == docselect)) and
     ((S2 == text) or
      (S2 == textarea))) -> DF;
% if the previous subcategory was of a type that is simple to convert
% to a string, do so. Take another pass through to deal with any
% consequences of this action.
update_normalize(#docfield{subcategory=S}, F, DF=#docfield{subcategory=S2,
                                                           value=V}) when 
    (((S == integer) or
      (S == rational) or
      (S == boolean) or
      (S == openboolean)) and
     ((S2 == text) or
      (S2 == textarea))) ->
    DF2 = DF#docfield{value=iolist_to_binary(io_lib:format("~p", [V]))},
    update_normalize(#docfield{subcategory=text}, F, DF2);
% Having established that the subcategory has changed, if the old
% subcategory is a date, convert the value to a string and make
% another pass through to sort out any issues that this move may have
% brought up.
update_normalize(#docfield{subcategory=date}, F, DF) ->
    update_normalize(#docfield{subcategory=text}, F, 
                     DF#docfield{
                       value=field:unconvert_value(date, DF#docfield.value)});
% There is no need to change anything if the change is from docselect
% to select or vice versa.
update_normalize(#docfield{subcategory=S}, _, DF=#docfield{subcategory=S2}) when
    ((S == docselect) or (S == select)) and 
    ((S2 == docselect) or (S2 == select)) -> DF;
% The same as above but for multiselects.
update_normalize(#docfield{subcategory=S}, _, DF=#docfield{subcategory=S2}) when
    ((S == docmultiselect) or (S == multiselect)) and 
    ((S2 == docmultiselect) or (S2 == multiselect)) -> DF;
% Convert from rationals to integers by truncating the fractional
% part.
update_normalize(#docfield{subcategory=rational}, _, 
                 DF=#docfield{subcategory=integer,value=V}) ->
    DF#docfield{value=erlang:trunc(V)};
% Convert integers to rationals by dividing by 1.
update_normalize(#docfield{subcategory=integer}, _, 
                 DF=#docfield{subcategory=rational,value=V}) -> 
    DF#docfield{value=(V / 1)};
% Going from boolean to openboolean will not cause any problems.
update_normalize(#docfield{subcategory=boolean}, _, 
                 DF=#docfield{subcategory=openboolean}) -> DF;
% Going from openboolean to boolean, set null to false.
update_normalize(#docfield{subcategory=openboolean}, _, 
                 DF=#docfield{subcategory=boolean,value=V}) ->
    DF#docfield{value=(V == true)};
% If none of the above condtions match, set the value to default.
update_normalize(_, #field{default=D}, DF) ->
    DF#docfield{value=D}.
