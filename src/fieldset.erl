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
%%% @doc This module contains functions for manipulating fieldsets

-module(fieldset).

-export([
         from_json/1,
         from_json/2,
         get/2,
         get/3,
         set_sortkeys/3,
         to_json/2,
         touch/3,
         touch_all/4
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("config.hrl").
-include_lib("types.hrl").

-spec touch_all([docfieldset()], [binary()], utils:reqdata(), any()) -> [docfieldset()].
touch_all(Dfss, FsIds, R, S) ->
    Dfss2 = add_missing(Dfss, FsIds, []),
    touch_all2(Dfss2, [], R, S).

touch_all2([], Acc, _R, _S) ->
    lists:reverse(Acc);
touch_all2([Dfs|Rest], Acc, R, S) ->
    case touch(Dfs, R, S) of
        undefined -> touch_all2(Rest, Acc, R, S);
        Val -> touch_all2(Rest, [Val|Acc], R, S)
    end.

%% @doc Given the current set of fieldsets in a document, a list of
%% ids and an empty accumulator, make sure that the current fieldsets
%% are complete according to the latest configuration and in the
%% correct order.
-spec add_missing([docfieldset()], [binary()], []) -> [docfieldset()].
add_missing(_Current, [], Complete) ->
    lists:reverse(Complete);
add_missing(Current, [Required|Rest], Acc) ->
    case find_required(Required, Current) of
        undefined ->
            add_missing(Current, Rest, [#docfieldset{id=Required}|Acc]);
        Found ->
            add_missing(Current, Rest, [Found|Acc])
    end.

%% @doc Search for a fieldset with a particular id in a list of
%% docfieldsets.
-spec find_required(binary(), [docfieldset()]) -> docfieldset() | undefined.
find_required(_, []) ->    
    undefined;
find_required(Id, [Found=#docfieldset{id=Id}|_]) ->
    Found;
find_required(Id, [_|Rest]) ->
    find_required(Id, Rest).
  
-spec touch(Dfs :: docfieldset(), R :: utils:reqdata(), S :: any()) -> Fieldset2 :: jsn:json_term().
touch(Dfs, R, S) ->
    case couch:get_view_json(
           binary_to_list(Dfs#docfieldset.id), "fields", R, S) of
        {ok, Fields} ->
            FieldsIds = [jsn:get_value(<<"id">>, X) || 
                            X <- jsn:get_value(<<"rows">>, Fields)],
            touch2(update(Dfs, R, S), FieldsIds, R, S);
        {error, req_timedout} -> touch(Dfs, R, S)
    end.
  
%% @doc Set the sortkeys for fields in fieldsets
-spec set_sortkeys(jsn:json_term() | [docfieldset()], R :: utils:reqdata(), S :: any()) -> jsn:json_term().
set_sortkeys([], _R, _S) ->
    [];
set_sortkeys(Fieldsets, R, S) when is_list(Fieldsets) ->
    set_sortkeys(Fieldsets, [], R, S).

%% @doc Convert a jsn:json_term() fieldset within a document to a
%% docfieldset() record.
-spec from_json(doc, Json :: jsn:json_term()) -> docfieldset().
from_json(doc, Json) ->
    Multiple = jsn:get_value(<<"multiple">>, Json),
    Fields = get_fields(Multiple, Json),
  
    #docfieldset{
                  id = jsn:get_value(<<"id">>, Json),
                  multiple = Multiple,
                  collapse = jsn:get_value(<<"collapse">>, Json),
                  name = jsn:get_value(<<"name">>, Json),
                  label = jsn:get_value(<<"label">>, Json),
                  order = jsn:get_value(<<"order">>, Json),
                  fields = Fields
                }.

%% @doc Convert a jsn:json_term() fieldset within a document to a
%% fieldset() record.
-spec from_json(Json :: jsn:json_term()) -> fieldset().
from_json(Json) ->
    #fieldset{
           id = jsn:get_value(<<"_id">>, Json),
           rev = jsn:get_value(<<"_rev">>, Json),
           category = fieldset,
           description = jsn:get_value(<<"description">>, Json),
           doctype = jsn:get_value(<<"doctype">>, Json),
           label = jsn:get_value(<<"label">>, Json),
           name = jsn:get_value(<<"name">>, Json),
           order = jsn:get_value(<<"order">>, Json),
           multiple = jsn:get_value(<<"multiple">>, Json),
           collapse = jsn:get_value(<<"collapse">>, Json)
          }.

%% @doc Convert a docfieldset() record within a document to a
%% jsn:json_term() fieldset.
-spec to_json(doc, FS :: docfieldset()) -> Json :: jsn:json_term().
to_json(doc, FS) ->
    Multiple = FS#docfieldset.multiple,
    [Fields] = get_fields(Multiple, FS),
  
    [{<<"id">>, FS#docfieldset.id},
     {<<"multiple">>, Multiple},
     {<<"collapse">>, FS#docfieldset.collapse},
     {<<"name">>, FS#docfieldset.name},
     {<<"label">>, FS#docfieldset.label},
     {<<"order">>, FS#docfieldset.order},
     Fields].

%% @doc Get a fieldset() using a fieldset id and a project id
-spec get(Project :: string(), Id :: string) -> fieldset().
get(Project, Id) ->
    Url = ?ADMINDB ++ Project ++ "/" ++ Id,
    {ok, "200", _, Json} = ibrowse:send_req(Url, [], get),
    from_json(jsn:decode(Json)).

%% @doc Get a fieldset() using a fieldset docfieldset() and webmachine state
-spec get(Dfs :: docfieldset(), R :: utils:reqdata(), S :: any()) -> fieldset().
get(Dfs, R, S) ->
    case proplists:get_value(table_id, S) of
        undefined -> get_from_couch(Dfs, R, S);
        Tid -> get_from_table(Tid, Dfs, R, S)
    end.

get_from_table(Tid, Dfs, R, S) ->
    case ets:lookup(Tid, Dfs#docfieldset.id) of
        [] ->
            Val = get_from_couch(Dfs, R, S),
            true = ets:insert(Tid, {Dfs#docfieldset.id, Val}),
            Val;
        [{_, Val}] -> Val
    end.
  
get_from_couch(Dfs, R, S) ->
    case couch:get_json(safer, binary_to_list(Dfs#docfieldset.id), R, S) of
        undefined -> undefined;
        {error, req_timedout} -> get(Dfs, R, S);
        Val -> from_json(Val)
    end.
  
-spec touch2(docfieldset() | undefined, [binary()], utils:reqdata(), any()) -> docfieldset() | undefined.
touch2(undefined, _Fids, _R, _S) ->
    undefined;
touch2(Dfs=#docfieldset{multiple=true,fields=undefined}, Fids, R, S) ->
    Dfs#docfieldset{fields=[field:touch_all(X, Fids, R, S) || X <- [[]]]};
touch2(Dfs=#docfieldset{multiple=true,fields=F}, Fids, R, S) ->
    Dfs#docfieldset{fields=[field:touch_all(X, Fids, R, S) || X <- F]};
touch2(Dfs=#docfieldset{multiple=false,fields=undefined}, Fids, R, S) ->
    Dfs#docfieldset{fields=field:touch_all([], Fids, R, S)};
touch2(Dfs=#docfieldset{multiple=false,fields=F}, Fids, R, S) ->
    Dfs#docfieldset{fields=field:touch_all(F, Fids, R, S)}.

-spec update(Dfs :: docfieldset(), R :: utils:reqdata(), S :: any()) -> docfieldset().
update(Dfs, R, S) ->
    case get(Dfs, R, S) of
        undefined -> undefined;
        Val -> update_merge(Dfs, Val)
    end.
  
-spec update_merge(Dfs :: docfieldset(), FS :: fieldset()) -> docfieldset() | {error, Reason :: term()}.
update_merge(Dfs, FS) ->
    case {FS#fieldset.id, Dfs#docfieldset.id} of
        {Id, Id} -> do_merge(Dfs, FS);
        _Else -> {error, "Id's do not match."}
    end.

-spec do_merge(Dfs :: docfieldset(), FS :: fieldset()) -> docfieldset() | {error, Reason :: term()}.
do_merge(Dfs, FS) ->
    Dfs#docfieldset{
      label = FS#fieldset.label,
      name = FS#fieldset.name,
      order = FS#fieldset.order,
      multiple = set_if_undefined(
                   Dfs#docfieldset.multiple, FS#fieldset.multiple),
      collapse = FS#fieldset.collapse
     }.

set_if_undefined(undefined, Val) ->
    Val;
set_if_undefined(Val, _) ->
    Val.

get_fields(false, FS=#docfieldset{}) ->
    [{<<"fields">>, [field:to_json(doc, X) || X <- FS#docfieldset.fields]}];
get_fields(true, FS=#docfieldset{}) ->
    [{<<"multifields">>, [get_fields(false, #docfieldset{fields = X}) || 
                             X <- FS#docfieldset.fields]}];
get_fields(false, Json) when is_list(Json) ->
    Ordering = fun (A, B) -> A#docfield.order =< B#docfield.order end,
    lists:sort(Ordering, [field:from_json(doc, X) || 
                             X <- jsn:get_value(<<"fields">>, Json)]);
get_fields(true, Json) when is_list(Json) ->
    [get_fields(false, X) || X <- jsn:get_value(<<"multifields">>, Json)].

set_sortkeys([], Acc, _R, _S) ->
    lists:reverse(Acc);
set_sortkeys([Fieldset|Rest], Acc, R, S) when is_list(Fieldset) ->
    {FSType, Val} = 
        case jsn:get_value(<<"multiple">>, Fieldset) of
            true ->
                {<<"multifields">>, 
                 multifields_sortkeys(
                   jsn:get_value(<<"multifields">>, Fieldset), R, S)};
            false -> 
                {<<"fields">>, 
                 field:set_sortkeys(
                   jsn:get_value(<<"fields">>, Fieldset), R, S)}
        end,
    set_sortkeys(Rest, [jsn:set_value(FSType, Val, Fieldset)|Acc], R, S);
set_sortkeys([FS=#docfieldset{}|Rest], Acc, R, S) ->
    Fields = 
        case FS#docfieldset.multiple of
            true ->
                [field:set_sortkeys(X, R, S) || X <- FS#docfieldset.fields];
            false -> 
                field:set_sortkeys(FS#docfieldset.fields, R, S)
        end,
    set_sortkeys(Rest, [FS#docfieldset{fields=Fields}|Acc], R, S).

multifields_sortkeys([], _R, _S) ->
    [];
multifields_sortkeys(Multifields, R, S) ->
    multifields_sortkeys(Multifields, [], R, S).

multifields_sortkeys([], Acc, _R, _S) ->
    lists:reverse(Acc);
multifields_sortkeys([Mfield|Rest], Acc, R, S) when is_list(Mfield) ->
    multifields_sortkeys(
      Rest, 
      [jsn:set_value(
         <<"fields">>, 
         field:set_sortkeys(jsn:get_value(<<"fields">>, Mfield), R, S), 
         Mfield)|Acc], R, S).
  
