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

index_info4() ->
    H = {<<"head">>, [<<"1">>]},
    R = {<<"reverse">>, [<<"6">>]},
    I = {<<"index">>, [{<<"1">>, [null,<<"one">>]},
                       {<<"2">>, [null,<<"two">>]},
                       {<<"3">>, [[null,<<"three">>],
                                  [null,<<"seven">>]]},
                       {<<"4">>, [[null,<<"four">>],
                                  [null,<<"eight">>]]},
                       {<<"5">>, [[null,<<"five">>],
                                  [null,<<"nine">>]]},
                       {<<"6">>, [[null,<<"six">>],
                                  [null,<<"ten">>]]}]},
    lists:reverse([R, H, I|lists:reverse(document_json4())]).

index_info2by4() ->
    H = {<<"head">>, [<<"1">>]},
    R = {<<"reverse">>, []},
    I = {<<"index">>, [{<<"1">>, [null,<<"one">>]},
                       {<<"2">>, [null,<<"two">>]},
                       {<<"3">>, [[null,<<"three">>],
                                  [null,<<"five">>],
                                  [null,<<"seven">>],
                                  [null,<<"nine">>]]},
                       {<<"4">>, [[null,<<"four">>],
                                  [null,<<"six">>],
                                  [null,<<"eight">>],
                                  [null,<<"ten">>]]}]},
    lists:reverse([R, H, I|lists:reverse(document_json2by4())]).

document_json4() ->
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
    Fifth = field_test:json_docfield([{<<"id">>, <<"5">>},
                                      {<<"value">>, <<"five">>},
                                      {<<"head">>, false},
                                      {<<"order">>, 5},
                                      {<<"subcategory">>, <<"text">>}]),
    Sixth = field_test:json_docfield([{<<"id">>, <<"6">>},
                                      {<<"value">>, <<"six">>},
                                      {<<"head">>, false},
                                      {<<"reversal">>, true},
                                      {<<"order">>, 6},
                                      {<<"subcategory">>, <<"text">>}]),
    Seventh = field_test:json_docfield(jsn:set_value(<<"value">>,
                                                     <<"seven">>, Third)),
    Eighth = field_test:json_docfield(jsn:set_value(<<"value">>,
                                                    <<"eight">>, Fourth)),
    Nineth = field_test:json_docfield(jsn:set_value(<<"value">>,
                                                    <<"nine">>, Fifth)),
    Tenth = field_test:json_docfield(jsn:set_value(<<"value">>,
                                                   <<"ten">>, Sixth)),
    Single = fieldset_test:single_json([First, Second]),
    Multiple = fieldset_test:multiple_json([[Third, Fourth, Fifth, Sixth],
                                            [Seventh, Eighth, Nineth, Tenth]]),
    [_,_,_,_|Doc] = lists:reverse(document_json()),
    Doc2 = lists:reverse([{<<"fieldsets">>, [Single, Multiple]}|Doc]),
    Doc2.

document_json2by4() ->
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
    Fifth = field_test:json_docfield(jsn:set_value(<<"value">>,
                                                   <<"five">>, Third)),
    Sixth = field_test:json_docfield(jsn:set_value(<<"value">>,
                                                   <<"six">>, Fourth)),
    Seventh = field_test:json_docfield(jsn:set_value(<<"value">>,
                                                     <<"seven">>, Third)),
    Eighth = field_test:json_docfield(jsn:set_value(<<"value">>,
                                                    <<"eight">>, Fourth)),
    Nineth = field_test:json_docfield(jsn:set_value(<<"value">>,
                                                    <<"nine">>, Third)),
    Tenth = field_test:json_docfield(jsn:set_value(<<"value">>,
                                                   <<"ten">>, Fourth)),
    Single = fieldset_test:single_json([First, Second]),
    Multiple = fieldset_test:multiple_json([[Third, Fourth], 
                                            [Fifth, Sixth],
                                            [Seventh, Eighth], 
                                            [Nineth, Tenth]]),
    [_,_,_,_|Doc] = lists:reverse(document_json()),
    Doc2 = lists:reverse([{<<"fieldsets">>, [Single, Multiple]}|Doc]),
    Doc2.
    
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

document2by4() ->
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
    Fifth = Third#docfield{value = <<"five">>},
    Sixth = Fourth#docfield{value = <<"six">>},
    Seventh = Third#docfield{value = <<"seven">>},
    Eighth = Fourth#docfield{value = <<"eight">>},
    Nineth = Third#docfield{value = <<"nine">>},
    Tenth = Fourth#docfield{value = <<"ten">>},
    
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
      fields = [[Third, Fourth], [Fifth, Sixth],
                [Seventh, Eighth], [Nineth, Tenth]]
     },
    D#document{
      fieldsets = [Single, Multiple],
      index = [{<<"1">>,[null,<<"one">>]},
               {<<"2">>,[null,<<"two">>]},
               {<<"3">>,[[null,<<"three">>],[null,<<"five">>],
                         [null, <<"seven">>], [null, <<"nine">>]]},
               {<<"4">>,[[null,<<"four">>],[null,<<"six">>],
                         [null,<<"eight">>],[null,<<"ten">>]]}],
      head = [<<"1">>],
      reverse = []
     }.

document4() ->
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
    Fifth = Second#docfield{id = <<"5">>, order = 5, value = <<"five">>},
    Sixth = Second#docfield{id = <<"6">>, 
                            order = 6,
                            reversal = true,
                            value = <<"six">>},
    Seventh = Third#docfield{value = <<"seven">>},
    Eighth = Fourth#docfield{value = <<"eight">>},
    Nineth = Fifth#docfield{value = <<"nine">>},
    Tenth = Sixth#docfield{value = <<"ten">>},
    
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
      fields = [[Third, Fourth, Fifth, Sixth],
                [Seventh, Eighth, Nineth, Tenth]]
     },
    D#document{
      fieldsets = [Single, Multiple],
      index = [{<<"1">>,[null,<<"one">>]},
               {<<"2">>,[null,<<"two">>]},
               {<<"3">>,[[null,<<"three">>],[null,<<"seven">>]]},
               {<<"4">>,[[null,<<"four">>],[null,<<"eight">>]]},
               {<<"5">>,[[null,<<"five">>],[null,<<"nine">>]]},
               {<<"6">>,[[null,<<"six">>],[null,<<"ten">>]]}],
      head = [<<"1">>],
      reverse = [<<"6">>]
     }.

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
    {"Convert from JSON term to record", 
     [
      {"Simple document",
       ?_assertEqual(document(), document:from_json(document_json()))},
      {"Simple document reverse",
       ?_assertEqual(element(15, document()), 
                     element(15, document:from_json(document_json())))},
      {"Simple document head",
       ?_assertEqual(element(14, document()), 
                     element(14, document:from_json(document_json())))},
      {"Simple document index",
       ?_assertEqual(element(13, document()), 
                     element(13, document:from_json(document_json())))},
      {"Simple document fieldset",
       ?_assertEqual(document2(), document:from_json(index_info2()))},
      {"Two two field multifieldset document", 
       ?_assertEqual(element(12, document2()),
                     element(12, document:from_json(index_info2())))},
      {"Two two field multifieldset document reverse",
       ?_assertEqual(element(15, document2()), 
                     element(15, document:from_json(index_info2())))},
      {"Two two field multifieldset document head", 
       ?_assertEqual(element(14, document2()), 
                     element(14, document:from_json(index_info2())))},
      {"Two two field multifieldset document index",
       ?_assertEqual(element(13, document2()), 
                     element(13, document:from_json(index_info2())))},
      {"Two four field multifieldset document", 
       ?_assertEqual(element(12, document4()),
                     element(12, document:from_json(index_info4())))},
      {"Two four field multifieldset document reverse",
       ?_assertEqual(element(15, document4()), 
                     element(15, document:from_json(index_info4())))},
      {"Two four field multifieldset document head", 
       ?_assertEqual(element(14, document4()), 
                     element(14, document:from_json(index_info4())))},
      {"Two four field multifieldset document index",
       ?_assertEqual(element(13, document4()), 
                     element(13, document:from_json(index_info4())))},
      {"Four two field multifieldset document", 
       ?_assertEqual(element(12, document2by4()),
                     element(12, document:from_json(index_info2by4())))},
      {"Four two field multifieldset document reverse",
       ?_assertEqual(element(15, document2by4()), 
                     element(15, document:from_json(index_info2by4())))},
      {"Four two field multifieldset document head", 
       ?_assertEqual(element(14, document2by4()), 
                     element(14, document:from_json(index_info2by4())))},
      {"Four two field multifieldset document index",
       ?_assertEqual(element(13, document2by4()), 
                     element(13, document:from_json(index_info2by4())))}
    ]}.

to_json_test_() ->
    {"Conversion from a record to a JSON term",
     [
      {"Whole simple document",
       ?_assertEqual(document_json(), document:to_json(document()))},
      {"Heads of simple document",
       ?_assertEqual(jsn:get_value(<<"head">>, document_json()),
                     jsn:get_value(<<"head">>, document:to_json(document())))},
      {"Reverses of simple document",
       ?_assertEqual(jsn:get_value(<<"reverse">>, document_json()),
                     jsn:get_value(<<"reverse">>, 
                                   document:to_json(document())))},
      {"Indexes of simple document",
       ?_assertEqual(jsn:get_value(<<"index">>, document_json()),
                     jsn:get_value(<<"index">>, document:to_json(document())))},
      {"Whole two two field multifieldset document",
       ?_assertEqual(index_info2(), document:to_json(document2()))},
      {"Heads of two two field multifieldset document",
       ?_assertEqual(jsn:get_value(<<"head">>, index_info2()),
                     jsn:get_value(<<"head">>, document:to_json(document2())))},
      {"Reverses of two two field multifieldset document",
       ?_assertEqual(jsn:get_value(<<"reverse">>, index_info2()),
                     jsn:get_value(<<"reverse">>, 
                                   document:to_json(document2())))},
      {"Indexes of two two field multifieldset document",
       ?_assertEqual(jsn:get_value(<<"index">>, index_info2()),
                     jsn:get_value(<<"index">>, 
                                   document:to_json(document2())))},
      {"Whole two four field multifieldset document",
       ?_assertEqual(index_info4(), document:to_json(document4()))},
      {"Heads of two four field multifieldset document",
       ?_assertEqual(jsn:get_value(<<"head">>, index_info4()),
                     jsn:get_value(<<"head">>, document:to_json(document4())))},
      {"Reverses of two four field multifieldset document",
       ?_assertEqual(jsn:get_value(<<"reverse">>, index_info4()),
                     jsn:get_value(<<"reverse">>, 
                                   document:to_json(document4())))},
      {"Indexes of two four field multifieldset document",
       ?_assertEqual(jsn:get_value(<<"index">>, index_info4()),
                     jsn:get_value(<<"index">>, 
                                   document:to_json(document4())))},
      {"Whole four two field multifieldset document",
       ?_assertEqual(index_info2by4(), document:to_json(document2by4()))},
      {"Heads of four two field multifieldset document",
       ?_assertEqual(jsn:get_value(<<"head">>, index_info2by4()),
                     jsn:get_value(<<"head">>, 
                                   document:to_json(document2by4())))},
      {"Reverses of four two field multifieldset document",
       ?_assertEqual(jsn:get_value(<<"reverse">>, index_info2by4()),
                     jsn:get_value(<<"reverse">>, 
                                   document:to_json(document2by4())))},
      {"Indexes of four two field multifieldset document",
       ?_assertEqual(jsn:get_value(<<"index">>, index_info2by4()),
                     jsn:get_value(<<"index">>, 
                                   document:to_json(document2by4())))}
     ]}.
 
normalize_test_() ->
    {"Normalization tests",
      [
       {"Simple document",
        ?_assertEqual(document_json(), document:normalize(document_json()))},
       {"Two two field multifieldsets",
        ?_assertEqual(index_info2(), document:normalize(index_info2()))},
       {"Two four field multifieldsets",
        ?_assertEqual(index_info4(), document:normalize(index_info4()))},
       {"Four two field multifieldsets",
        ?_assertEqual(index_info2by4(),document:normalize(index_info2by4()))}
      ]}.
