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
  merge_update/2,
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
    max = convert_value(Subcategory, jsn:get_value(<<"max">>, Json)),
    min = convert_value(Subcategory, jsn:get_value(<<"min">>, Json)),
    regex = jsn:get_value(<<"regex">>, Json),
    required = jsn:get_value(<<"required">>, Json),
    reversal = jsn:get_value(<<"reversal">>, Json),
    subcategory = Subcategory,
    value = convert_value(Subcategory, jsn:get_value(<<"value">>, Json)),
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
  {<<"min">>, unconvert_value(F#docfield.subcategory, F#docfield.min)},
  {<<"max">>, unconvert_value(F#docfield.subcategory, F#docfield.max)},
  {<<"instance">>, F#docfield.instance},
  {<<"charseq">>, maybe_binary(F#docfield.charseq)},
  {<<"regex">>, F#docfield.regex},
  {<<"order">>, F#docfield.order},
  {<<"subcategory">>, atom_to_binary(F#docfield.subcategory, utf8)},
  {<<"value">>, unconvert_value(F#docfield.subcategory, F#docfield.value)},
  {<<"sortkey">>, F#docfield.sortkey}].
  
%% @doc Get a field() using a field id and a project id

-spec get(Project :: string(), Id :: string) -> fieldset() | {error, Reason :: term()}.
get(Project, Id) ->
  Url = ?ADMINDB ++ Project ++ "/" ++ Id,
  case ibrowse:send_req(Url, [], get) of
    {ok, "200", _, Raw} -> convert_field(jsn:decode(Raw));
    {ok, Status, _, _} -> {error, "HTTP status " ++ Status};
    Error -> Error
  end.

%% @doc Take a field() and a docfield() and return a docfiled() updated so
%% that shared items match and sortkey is updated.

-spec merge_update(F :: field(), DF :: docfield()) -> docfield() | {error, Reason :: term()}.
merge_update(F, DF) ->
  case {F#field.id, DF#docfield.id} of
    {Id, Id} -> do_merge(F, DF);
    _Else -> {error, "Id's do not match."}
  end.

-spec maybe_binary(B :: term()) -> binary() | null.
maybe_binary(<<>>) -> null;
maybe_binary(B) when is_binary(B) -> B;
maybe_binary(_) -> null.

-spec unconvert_value(subcategory(), Value :: term()) -> jsn:json_term().
unconvert_value(date, today) -> <<"today">>;
unconvert_value(date, {Y, M, D}) ->
  list_to_binary(string:right(integer_to_list(Y), 4, $0) ++ "-" ++ string:right(integer_to_list(M), 2, $0) ++ "-" ++ string:right(integer_to_list(D), 2, $0));
unconvert_value(_, Value) -> Value.

-spec convert_value(subcategory(), Value :: jsn:json_term()) -> anyval().
convert_value(date, <<"today">>) -> today;
convert_value(date, Value) when is_binary(Value) ->
  Value1 = binary_to_list(Value),
  case length(Value1) of
    10 -> parse_date(Value1);
    _ -> null
  end;
convert_value(_, Value) -> Value.

-spec parse_date(string()) -> calendar:date().
parse_date([Y1,Y2,Y3,Y4,_,M1,M2,_,D1,D2]) ->
  {list_to_integer([Y1,Y2,Y3,Y4]), list_to_integer([M1,M2]), list_to_integer([D1,D2])}.

-spec do_merge(F :: field(), DF :: docfield()) -> docfield() | {error, Reason :: term()}.
do_merge(F, DF) ->
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
  update_normalize(DF, F, DF2).
  
-spec update_normalize(subcategory(), F :: field(), DF2 :: docfield()) -> docfield() | {error, Reason :: term()}.
update_normalize(_, _, DF) -> DF.    
  
-spec convert_field(Json :: jsn:json_term()) -> fieldset() | {error, Reason :: term()}.
convert_field(Json) ->  
  case jsn:get_value(<<"category">>, Json) of
    <<"field">> -> from_json(Json);
    Cat -> {error, "Returned document had category " ++ binary_to_list(Cat)}
  end.
       
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
