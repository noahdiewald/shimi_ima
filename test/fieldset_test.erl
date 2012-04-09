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
%%% @doc Tests for fieldset functions

-module(fieldset_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("types.hrl").
    
single_json() -> 
  [{<<"id">>,<<"25250e2ead108a8f60213f24040007e4">>},
  {<<"multiple">>,false},
  {<<"collapse">>,true},
  {<<"name">>,<<"caltest">>},
  {<<"label">>,<<"CalTest">>},
  {<<"order">>,1},
  {<<"fields">>,[field_test:json_docfield(),field_test:json_docfield()]}].

single_json(Fields) ->
    jsn:set_value(<<"fields">>, Fields, single_json()).
    
single_docfieldset() -> 
  #docfieldset{
    id = <<"25250e2ead108a8f60213f24040007e4">>,
    multiple = false,
    collapse = true,
    name = <<"caltest">>,
    label = <<"CalTest">>,
    order = 1,
    fields = [field_test:docfield(), field_test:docfield()]}.

multiple_json() -> 
  [{<<"id">>,<<"25250e2ead108a8f60213f24040007e4">>},
  {<<"multiple">>,true},
  {<<"collapse">>,false},
  {<<"name">>,<<"caltest">>},
  {<<"label">>,<<"CalTest">>},
  {<<"order">>,2},
  {<<"multifields">>,[
    [{<<"fields">>,[field_test:json_docfield(),field_test:json_docfield()]}],
    [{<<"fields">>,[field_test:json_docfield(),field_test:json_docfield()]}]
   ]}].

multiple_json(Fields) ->
    multiple_json(Fields, []).

multiple_json([], Acc) ->
    jsn:set_value(<<"multifields">>, lists:reverse(Acc), multiple_json());
multiple_json([Fields|Rest], Acc) ->
    multiple_json(Rest, [[{<<"fields">>, Fields}]|Acc]).

multiple_docfieldset() -> 
  #docfieldset{
    id = <<"25250e2ead108a8f60213f24040007e4">>,
    multiple = true,
    collapse = false,
    name = <<"caltest">>,
    label = <<"CalTest">>,
    order = 2,
    fields = [
      [field_test:docfield(), field_test:docfield()],
      [field_test:docfield(), field_test:docfield()]
    ]}.

from_json_test_() ->
 [
   ?_assertEqual(fieldset:from_json(doc, single_json()), single_docfieldset()),
   ?_assertEqual(fieldset:from_json(doc, multiple_json()), multiple_docfieldset())
 ].

to_json_test_() ->
 [
   ?_assertEqual(fieldset:to_json(doc, single_docfieldset()), single_json()),
   ?_assertEqual(fieldset:to_json(doc, multiple_docfieldset()), multiple_json())
 ].
 
