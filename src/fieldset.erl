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
         set_sortkeys/3,
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("config.hrl").
-include_lib("types.hrl").
  
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
  
