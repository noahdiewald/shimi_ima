%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
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
%%% @doc Helpers for providing and accepting data in a format suitable
%%% for the worksheet multiple editing form.

-module(h).

-include_lib("types.hrl").

-export([
         get/4
        ]).

-spec fmt_fieldsets([docfieldset()], jsn:json_term()) -> jsn:json_term().
fmt_fieldsets([], Acc) ->
    Acc;
fmt_fieldsets([H=#docfieldset{multiple=true}|T], Acc) ->
    fmt_fieldsets(T, lists:foldl(fun fmt_multifields/2, H#docfieldset.fields, Acc));
fmt_fieldsets([H=#docfieldset{multiple=false}|T], Acc) ->
    FSInstance = list_to_binary(utils:uuid()),
    Fun = fun (F, X) ->
                  FInstance = list_to_binary(utils:uuid()),
                  New = [{<<"fieldset_instance">>, FSInstance},
                         {<<"field_instance">>, FInstance},
                         {<<"value">>, F#docfield.value}],
                  jsn:set_value(F#docfield.id, [{<<"single">>, New}])
          end,
    fmt_fieldsets(T, lists:foldl(Fun, H#docfieldset.fields, Acc)).
    
-spec fmt_multifields([docfields()], jsn:json_term()) -> jsn:json_term().
fmt_multifields(Fields, Acc) ->
    FSInstance = list_to_binary(utils:uuid()),
    Fun = fun (F, X) ->
                  FInstance = list_to_binary(utils:uuid()),
                  Prev = proplists:get_value(F#docfield.id, X, [{<<"multiple">>, [{<<"items">>, []}]}]),
                  Items = jsn:get_value(<<"items">>, jsn:get_value(<<"multiple">>, Prev)),
                  New = [{<<"fieldset_instance">>, FSInstance},
                         {<<"field_instance">>, FInstance},
                         {<<"value">>, F#docfield.value}],
                  jsn:set_value(F#docfield.id, [{<<"multiple">>, [{<<"items">>, Items ++ [New]}]}])
          end,
    lists:foldl(Fun, Fields, Acc).

-spec get(string(), string(), string(), any()) -> jsn:json_term().
get(Docs, Project, S) ->
    VQ = #vq{keys =  Docs, include_docs = true},
    Qs = view:to_string(VQ),
    {ok, Json} = q:index(Qs, Project, S),
    trans_from(Json).

-spec trans_from(jsn:json_term()) -> jsn:json_term().
trans_from(Json) ->
    Transed = trans_from(jsn:get_value(<<"rows">>, Json), []),
    jsn:set_value(<<"rows">>, Transed).

-spec trans_from([jsn:json_term()], [jsn:json_term()]) -> jsn:json_term().
trans_from([], Acc) ->
    lists:reverse(Acc);
trans_from([H|T], Acc) ->
    Doc = document:from_json(doc, jsn:get_value(<<"doc">>, H)),
    Head = Doc#document.head,
    Id = Doc#document.id,
    Rev = Doc#document.rev,
    New = fmt_fieldsets(Doc#document.fieldsets, []),
    [{<<"head">>, Head}, {<<"_id">>, Id}, {<<"_rev">>, Rev}|New].
