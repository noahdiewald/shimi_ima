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
  from_json/2,
  to_json/2
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").
-include_lib("include/types.hrl").

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
  [{<<"multifields">>, [get_fields(false, #docfieldset{fields = X}) || X <- FS#docfieldset.fields]}];
get_fields(false, Json) when is_list(Json) ->
  [field:from_json(doc, X) || X <- jsn:get_value(<<"fields">>, Json)];
get_fields(true, Json) when is_list(Json) ->
  [get_fields(false, X) || X <- jsn:get_value(<<"multifields">>, Json)].
  