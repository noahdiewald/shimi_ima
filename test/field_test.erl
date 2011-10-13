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
%%% @doc Tests for fields functions

-module(field_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/types.hrl").

json_field() ->
  [{<<"id">>,<<"25250e2ead108a8f60213f24040007e4">>},
    {<<"name">>,<<"caltest">>},
    {<<"label">>,<<"CalTest">>},
    {<<"head">>,true},
    {<<"reversal">>,false},
    {<<"required">>,false},
    {<<"min">>,<<"2001-09-27">>},
    {<<"max">>,<<"2010-08-15">>},
    {<<"instance">>,<<"25250e2ead108a8f60213f2404016d15">>},
    {<<"charseq">>,<<>>},
    {<<"regex">>,<<>>},
    {<<"order">>,50},
    {<<"subcategory">>,<<"date">>},
    {<<"value">>,<<"2009-08-23">>},
    {<<"sortkey">>,<<>>}].

docfield() ->
  #docfield{
    id = <<"25250e2ead108a8f60213f24040007e4">>,
    instance = <<"25250e2ead108a8f60213f2404016d15">>,
    charseq = <<>>,
    name = <<"caltest">>,
    label = <<"CalTest">>,
    head = true,
    reversal = false,
    required = false,
    min = <<"2001-09-27">>,
    max = <<"2010-08-15">>,
    regex = <<>>,
    order = 50,
    subcategory = date,
    value = <<"2009-08-23">>,
    sortkey = <<>>
  }.

from_json_test_() ->
 [
   ?_assertEqual(field:from_json(doc, json_field()), docfield())
 ].

to_json_test_() ->
 [
   ?_assertEqual(lists:reverse(field:to_json(doc, docfield())), lists:reverse(json_field()))
 ].
