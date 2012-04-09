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

-module(document_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("types.hrl").
    
document_json() -> 
    [{<<"_id">>,<<"25250e2ead108a8f60213f24040007e4">>},
     {<<"_rev">>,<<"25250e2ead108a8f60213f24040007e4">>},
     {<<"description">>,<<"caltest">>},
     {<<"doctype">>,<<"CalTest">>},
     {<<"created_at_">>,<<"Tue, 08 Nov 2011 21:24:04 GMT">>},
     {<<"created_by_">>,<<"257e4">>},
     {<<"updated_at_">>,<<"Tue, 08 Nov 2011 21:24:04 GMT">>},
     {<<"updated_by_">>,<<"25250e4">>},
     {<<"prev_">>,<<"25250e2ead108a8f60213f24040007e4">>},
     {<<"deleted_">>,false},
     {<<"fieldsets">>,[fieldset_test:single_json(),
                       fieldset_test:multiple_json()]},
     {<<"index">>, [{<<"25250e2ead108a8f60213f24040007e4">>, 
                     [null, <<"2009-08-23">>]},
                    {<<"25250e2ead108a8f60213f24040007e4">>, 
                     [null, <<"2009-08-23">>]},
                    {<<"25250e2ead108a8f60213f24040007e4">>, 
                     [[null, <<"2009-08-23">>], [null, <<"2009-08-23">>]]},
                    {<<"25250e2ead108a8f60213f24040007e4">>, 
                     [[null, <<"2009-08-23">>], [null, <<"2009-08-23">>]]}]},
     {<<"head">>, [<<"25250e2ead108a8f60213f24040007e4">>,
                   <<"25250e2ead108a8f60213f24040007e4">>,
                   <<"25250e2ead108a8f60213f24040007e4">>,
                   <<"25250e2ead108a8f60213f24040007e4">>]},
     {<<"reverse">>, []}].

index_info2() ->
    H = {<<"head">>, [<<"1">>]},
    R = {<<"reverse">>, []},
    I = {<<"index">>, [{<<"1">>, [null,<<"one">>]},
                       {<<"2">>, [null,<<"two">>]},
                       {<<"3">>, [[null,<<"three">>],
                                  [null,<<"five">>]]},
                       {<<"4">>, [[null,<<"four">>],
                                  [null,<<"six">>]]}]},
    lists:reverse([R, H, I|lists:reverse(document_json2())]).

document_json2() ->
    First = field_test:json_docfield([{<<"id">>, <<"1">>},
                                      {<<"value">>, <<"one">>},
                                      {<<"order">>, 1},
                                      {<<"subcategory">>, <<"text">>}]),
    Second = field_test:json_docfield([{<<"id">>, <<"2">>},
                                       {<<"value">>, <<"two">>},
                                       {<<"head">>, false},
                                       {<<"order">>, 2},
                                       {<<"subcategory">>, <<"text">>}]),
    Third = field_test:json_docfield([{<<"id">>, <<"3">>},
                                      {<<"value">>, <<"three">>},
                                      {<<"head">>, false},
                                      {<<"order">>, 3},
                                      {<<"subcategory">>, <<"text">>}]),
    Fourth = field_test:json_docfield([{<<"id">>, <<"4">>},
                                       {<<"value">>, <<"four">>},
                                       {<<"head">>, false},
                                      {<<"order">>, 4},
                                       {<<"subcategory">>, <<"text">>}]),
    Fifth = field_test:json_docfield([{<<"id">>, <<"3">>},
                                      {<<"value">>, <<"five">>},
                                      {<<"head">>, false},
                                      {<<"order">>, 3},
                                      {<<"subcategory">>, <<"text">>}]),
    Sixth = field_test:json_docfield([{<<"id">>, <<"4">>},
                                      {<<"value">>, <<"six">>},
                                      {<<"head">>, false},
                                      {<<"order">>, 4},
                                      {<<"subcategory">>, <<"text">>}]),
    Single = fieldset_test:single_json([First, Second]),
    Multiple = fieldset_test:multiple_json([[Third, Fourth], [Fifth, Sixth]]),
    [_,_,_,_|Doc] = lists:reverse(document_json()),
    Doc2 = lists:reverse([{<<"fieldsets">>, [Single, Multiple]}|Doc]),
    Doc2.

document2() ->
    D = document(),

    First = #docfield{
      id = <<"1">>,
      instance = <<"25250e2ead108a8f60213f2404016d15">>,
      charseq = null,
      name = <<"caltest">>,
      label = <<"CalTest">>,
      head = true,
      reversal = false,
      required = false,
      min = <<"2001-09-27">>,
      max = <<"2010-08-15">>,
      regex = null,
      order = 1,
      subcategory = text,
      value = <<"one">>,
      sortkey = null
     },
    Second = First#docfield{
               id = <<"2">>,
               head = false,
               order = 2,
               value = <<"two">>
              },
    Third = Second#docfield{id = <<"3">>, order = 3, value = <<"three">>},
    Fourth = Second#docfield{id = <<"4">>, order = 4, value = <<"four">>},
    Fifth = Third#docfield{id = <<"3">>, value = <<"five">>},
    Sixth = Fourth#docfield{id = <<"4">>, value = <<"six">>},
    
    Single = #docfieldset{
      id = <<"25250e2ead108a8f60213f24040007e4">>,
      multiple = false,
      collapse = true,
      name = <<"caltest">>,
      label = <<"CalTest">>,
      order = 1,
      fields = [First, Second]
     },
    Multiple = #docfieldset{
      id = <<"25250e2ead108a8f60213f24040007e4">>,
      multiple = true,
      collapse = false,
      name = <<"caltest">>,
      label = <<"CalTest">>,
      order = 2,
      fields = [[Third, Fourth],[Fifth, Sixth]]
     },
    D#document{
      fieldsets = [Single, Multiple],
      index = [{<<"1">>,[null,<<"one">>]},
               {<<"2">>,[null,<<"two">>]},
               {<<"3">>,[[null,<<"three">>],[null,<<"five">>]]},
               {<<"4">>,[[null,<<"four">>],[null,<<"six">>]]}],
      head = [<<"1">>],
      reverse = []
     }.

document() -> 
    #document{
          id = <<"25250e2ead108a8f60213f24040007e4">>,
          rev = <<"25250e2ead108a8f60213f24040007e4">>,
          description = <<"caltest">>,
          doctype = <<"CalTest">>,
          prev = <<"25250e2ead108a8f60213f24040007e4">>,
          created_by = <<"257e4">>,
          updated_by = <<"25250e4">>,
          deleted = false,
          created_at = {{2011,11,8},{21,24,04}},
          updated_at = {{2011,11,8},{21,24,04}},
          fieldsets = [fieldset_test:single_docfieldset(), 
                       fieldset_test:multiple_docfieldset()],
          index = [{<<"25250e2ead108a8f60213f24040007e4">>, 
                    [null, {2009,8,23}]},
                   {<<"25250e2ead108a8f60213f24040007e4">>, 
                    [null, {2009,8,23}]},
                   {<<"25250e2ead108a8f60213f24040007e4">>, 
                    [[null, {2009,8,23}], [null, {2009,8,23}]]},
                   {<<"25250e2ead108a8f60213f24040007e4">>, 
                    [[null, {2009,8,23}], [null, {2009,8,23}]]}],
          head = [<<"25250e2ead108a8f60213f24040007e4">>,
                  <<"25250e2ead108a8f60213f24040007e4">>,
                  <<"25250e2ead108a8f60213f24040007e4">>,
                  <<"25250e2ead108a8f60213f24040007e4">>],
          reverse = []
         }.

from_json_test_() ->
    [
     ?_assertEqual(document(), document:from_json(document_json())),
     ?_assertEqual(element(15, document()), 
                   element(15, document:from_json(document_json()))),
     ?_assertEqual(element(14, document()), 
                   element(14, document:from_json(document_json()))),
     ?_assertEqual(element(13, document()), 
                   element(13, document:from_json(document_json()))),
     ?_assertEqual(document2(), document:from_json(index_info2())),
     ?_assertEqual(element(12, document2()),
                   element(12, document:from_json(index_info2()))),
     ?_assertEqual(element(15, document2()), 
                   element(15, document:from_json(index_info2()))),
     ?_assertEqual(element(14, document2()), 
                   element(14, document:from_json(index_info2()))),
     ?_assertEqual(element(13, document2()), 
                   element(13, document:from_json(index_info2())))
    ].

to_json_test_() ->
    [
     ?_assertEqual(document_json(), document:to_json(document())),
     ?_assertEqual(jsn:get_value(<<"head">>, document_json()),
                   jsn:get_value(<<"head">>, document:to_json(document()))),
     ?_assertEqual(jsn:get_value(<<"reverse">>, document_json()),
                   jsn:get_value(<<"reverse">>, document:to_json(document()))),
     ?_assertEqual(jsn:get_value(<<"index">>, document_json()),
                   jsn:get_value(<<"index">>, document:to_json(document()))),
     ?_assertEqual(index_info2(), document:to_json(document2())),
     ?_assertEqual(jsn:get_value(<<"head">>, index_info2()),
                   jsn:get_value(<<"head">>, document:to_json(document2()))),
     ?_assertEqual(jsn:get_value(<<"reverse">>, index_info2()),
                   jsn:get_value(<<"reverse">>, document:to_json(document2()))),
     ?_assertEqual(jsn:get_value(<<"index">>, index_info2()),
                   jsn:get_value(<<"index">>, document:to_json(document2())))
    ].
 
normalize_test_() ->
    [
     ?_assertEqual(document_json(), document:normalize(document_json())),
     ?_assertEqual(index_info2(), document:normalize(index_info2()))
    ].
