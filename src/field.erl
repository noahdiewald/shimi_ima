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
%%% @doc This module contains functions for manipulating fields

-module(field).

-export([
  from_json/1,
  from_json/2,
  get/2,
  set_sortkeys/3,
  to_json/2
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").
-include_lib("include/types.hrl").

%% @doc Set the sortkeys for fields

-spec set_sortkeys(jsn:json_term(), R :: utils:reqdata(), S :: any()) -> jsn:json_term().
set_sortkeys([], _R, _S) ->
  [];
set_sortkeys(Fields, R, S) ->
  set_sortkeys(Fields, [], R, S).

%% @doc Convert a jsn:json_term() field to a field()

-spec from_json(Json :: jsn:json_term()) -> docfield().
from_json(Json) ->
  Subcategory = get_subcategory(jsn:get_value(<<"subcategory">>, Json)),
  #field{
    id = jsn:get_value(<<"_id">>, Json),
    rev = jsn:get_value(<<"_rev">>, Json),
    allowed = jsn:get_value(<<"allowed">>, Json),
    category = field,
    charseq = jsn:get_value(<<"charseq">>, Json),
    default = jsn:get_value(<<"default">>, Json),
    description = jsn:get_value(<<"description">>, Json),
    doctype = jsn:get_value(<<"doctype">>, Json),
    fieldset = jsn:get_value(<<"fieldset">>, Json),
    head = jsn:get_value(<<"head">>, Json),
    label = jsn:get_value(<<"label">>, Json),
    max = jsn:get_value(<<"max">>, Json),
    min = jsn:get_value(<<"min">>, Json),
    name = jsn:get_value(<<"name">>, Json),
    order = jsn:get_value(<<"order">>, Json),
    regex = jsn:get_value(<<"regex">>, Json),
    required = jsn:get_value(<<"required">>, Json),
    reversal = jsn:get_value(<<"reversal">>, Json),
    source = jsn:get_value(<<"source">>, Json),
    subcategory = Subcategory
  }.

%% @doc Convert a jsn:json_term() field within a document to a
%% docfield()

-spec from_json(doc, Json :: jsn:json_term()) -> docfield().
from_json(doc, Json) ->
  Subcategory = get_subcategory(jsn:get_value(<<"subcategory">>, Json)),
  #docfield{
    id = jsn:get_value(<<"id">>, Json),
    instance = jsn:get_value(<<"instance">>, Json),
    charseq = jsn:get_value(<<"charseq">>, Json),
    name = jsn:get_value(<<"name">>, Json),
    label = jsn:get_value(<<"label">>, Json),
    order = jsn:get_value(<<"order">>, Json),
    head = jsn:get_value(<<"head">>, Json),
    max = jsn:get_value(<<"max">>, Json),
    min = jsn:get_value(<<"min">>, Json),
    regex = jsn:get_value(<<"regex">>, Json),
    required = jsn:get_value(<<"required">>, Json),
    reversal = jsn:get_value(<<"reversal">>, Json),
    subcategory = Subcategory,
    value = jsn:get_value(<<"value">>, Json),
    sortkey = jsn:get_value(<<"sortkey">>, Json)
  }.

%% @doc Convert a docfield() record within a document() to a
%% jsn:json_term() fieldset 

-spec to_json(doc, F :: docfield()) -> Json :: jsn:json_term().
to_json(doc, F) ->
  [{<<"id">>, F#docfield.id},
  {<<"name">>, F#docfield.name},
  {<<"label">>, F#docfield.label},
  {<<"head">>, F#docfield.head},
  {<<"reversal">>, F#docfield.reversal},
  {<<"required">>, F#docfield.required},
  {<<"min">>, F#docfield.min},
  {<<"max">>, F#docfield.max},
  {<<"instance">>, F#docfield.instance},
  {<<"charseq">>, F#docfield.charseq},
  {<<"regex">>, F#docfield.regex},
  {<<"order">>, F#docfield.order},
  {<<"subcategory">>, atom_to_binary(F#docfield.subcategory, utf8)},
  {<<"value">>, F#docfield.value},
  {<<"sortkey">>, F#docfield.sortkey}].

%% @doc Get a field() using a field id and a project id

-spec get(Project :: string(), Id :: string) -> fieldset().
get(Project, Id) ->
  Url = ?ADMINDB ++ Project ++ "/" ++ Id,
  {ok, "200", _, Json} = ibrowse:send_req(Url, [], get),
  from_json(jsn:decode(Json)).
       
-spec get_subcategory(binary()) -> subcategory().
get_subcategory(Bin) ->
  case Bin of
    <<"text">> -> text;
    <<"textarea">> -> textarea;
    <<"date">> -> date;
    <<"integer">> -> integer;
    <<"rational">> -> rational;
    <<"boolean">> -> boolean;
    <<"openboolean">> -> openboolean;
    <<"select">> -> select;
    <<"multiselect">> -> multiselect;
    <<"docselect">> -> docselect;
    <<"docmultiselect">> -> docmultiselect;
    <<"file">> -> file
  end.

set_sortkeys([], Acc, _R, _S) ->
  lists:reverse(Acc);
set_sortkeys([Field|Rest], Acc, R, S) when is_list(Field) ->
  set_sortkeys(Rest, [jsn:set_value(<<"sortkey">>, charseq:get_sortkey(Field, R, S), Field)|Acc], R, S);
set_sortkeys([F=#docfield{}|Rest], Acc, R, S) ->
  set_sortkeys(Rest, [F#docfield{sortkey=charseq:get_sortkey(F, R, S)}|Acc], R, S).
