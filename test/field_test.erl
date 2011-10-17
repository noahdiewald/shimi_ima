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
  [{<<"_id">>,<<"25250e2ead108a8f60213f24040007e4">>},
    {<<"_rev">>,<<"25250e2ead108a8f60213f24040007e4">>},
    {<<"allowed">>,null},
    {<<"category">>,<<"field">>},
    {<<"charseq">>,null},
    {<<"default">>,null},
    {<<"description">>,null},
    {<<"doctype">>,<<"caltest">>},
    {<<"fieldset">>,<<"caltest">>},
    {<<"head">>,true},
    {<<"label">>,<<"CalTest">>},
    {<<"max">>,<<"2010-08-15">>},
    {<<"min">>,<<"2001-09-27">>},
    {<<"name">>,<<"caltest">>},
    {<<"order">>,50},
    {<<"regex">>,null},
    {<<"required">>,false},
    {<<"reversal">>,false},
    {<<"source">>,null},
    {<<"subcategory">>,<<"date">>}].

field() ->
  #field{
    id = <<"25250e2ead108a8f60213f24040007e4">>,
    rev = <<"25250e2ead108a8f60213f24040007e4">>,
    allowed = null,
    category = field,
    charseq = null,
    default = null,
    description = null,
    doctype = <<"caltest">>,
    fieldset = <<"caltest">>,
    head = true,
    label = <<"CalTest">>,
    max = {2010,8,15},
    min = {2001,9,27},
    name = <<"caltest">>,
    order = 50,
    regex = null,
    required = false,
    reversal = false,
    source = null,
    subcategory = date
  }.

json_docfield() ->
  [{<<"id">>,<<"25250e2ead108a8f60213f24040007e4">>},
    {<<"name">>,<<"caltest">>},
    {<<"label">>,<<"CalTest">>},
    {<<"head">>,true},
    {<<"reversal">>,false},
    {<<"required">>,false},
    {<<"min">>,<<"2001-09-27">>},
    {<<"max">>,<<"2010-08-15">>},
    {<<"instance">>,<<"25250e2ead108a8f60213f2404016d15">>},
    {<<"charseq">>,null},
    {<<"regex">>,null},
    {<<"order">>,50},
    {<<"subcategory">>,<<"date">>},
    {<<"value">>,<<"2009-08-23">>},
    {<<"sortkey">>,null}].
  
docfield() ->
  #docfield{
    id = <<"25250e2ead108a8f60213f24040007e4">>,
    instance = <<"25250e2ead108a8f60213f2404016d15">>,
    charseq = null,
    name = <<"caltest">>,
    label = <<"CalTest">>,
    head = true,
    reversal = false,
    required = false,
    min = {2001, 9, 27},
    max = {2010, 8, 15},
    regex = null,
    order = 50,
    subcategory = date,
    value = {2009, 8, 23},
    sortkey = null
  }.

json_docfield_w_undefined() ->
  F = fun (Val, Acc) -> jsn:delete_value(Val, Acc) end,
  lists:foldr(F, json_docfield(), [<<"sortkey">>, <<"regex">>, <<"charseq">>]).

json_field_w_undefined() ->
  F = fun (Val, Acc) -> jsn:delete_value(Val, Acc) end,
  lists:foldr(F, json_field(), [<<"regex">>, <<"charseq">>]).

set_field_values([]) ->
  field();
set_field_values(KVs) ->
  field:from_json(set_field_values(KVs, json_field())).

set_docfield_values([]) ->
  docfield();
set_docfield_values(KVs) ->
  field:from_json(doc, set_field_values(KVs, json_docfield())).

set_field_values([], Acc) ->
  Acc;  
set_field_values([{Key, Value}|Rest], Acc) ->
  set_field_values(Rest, jsn:set_value(Key, Value, Acc)).

v(Fun, false) ->
  Fun();
v(Fun, true) ->
  V = Fun(),
  io:format("====| ~p |====", [V]),
  V.
  
from_json_test_() ->
 [
   % Basic docfield
   ?_assertEqual(field:from_json(doc, json_docfield()), docfield()),
   
   % Instance where the docfield JSON didn't define all values
   ?_assertEqual(field:from_json(doc, json_docfield_w_undefined()), docfield()),
   
   % from_json is the inverse of to_json
   ?_assertEqual(field:from_json(doc, json_docfield_w_undefined()),
                 field:from_json(doc, field:to_json(doc, docfield()))),

   % Basic field
   ?_assertEqual(field:from_json(json_field()), field()),
   
   % Instance where the field JSON didn't define all values
   ?_assertEqual(field:from_json(json_field_w_undefined()), field())
 ].

to_json_test_() ->
 [
   ?_assertEqual(lists:reverse(field:to_json(doc, docfield())), lists:reverse(json_docfield()))
 ].

merge_update_test_() ->
[
  % If the IDs don't match, there should be an error
  ?_assertEqual({error, "Id's do not match."},
                field:merge_update(set_field_values([{<<"_id">>, <<"tfdsadext">>}]), docfield())),
                
  % If parameter changes, it should be reflected in the docfield
  ?_assertEqual(field:from_json(doc, jsn:set_value(<<"head">>, false, json_docfield())),
                field:merge_update(set_field_values([{<<"head">>, false}]), docfield())),
  
  % If the subcategory changes in the field, those changes 
  % should be reflected in the docfield's values. Date to text test.
  ?_assertEqual(
    v(fun () ->
      set_docfield_values([{<<"subcategory">>, <<"text">>},
                           {<<"min">>, null},
                           {<<"max">>, null},
                           {<<"value">>, <<"9.0">>}])
    end, true),
    v(fun () ->
      field:merge_update(set_field_values([{<<"subcategory">>, <<"text">>},
                                           {<<"min">>, null},
                                           {<<"max">>, null}]),
                         set_docfield_values([{<<"subcategory">>, <<"rational">>},
                                              {<<"min">>, null},
                                              {<<"max">>, null},
                                              {<<"value">>, 9.0}]))
    end, true)),
  
  % If the subcategory changes in the field, those changes 
  % should be reflected in the docfield's values. True openoolean to text test.
  ?_assertEqual(
    v(fun () ->
      set_docfield_values([{<<"subcategory">>, <<"text">>},
                           {<<"min">>, null},
                           {<<"max">>, null},
                           {<<"value">>, <<"true">>}])
    end, true),
    v(fun () ->
      field:merge_update(set_field_values([{<<"subcategory">>, <<"text">>},
                                           {<<"min">>, null},
                                           {<<"max">>, null}]),
                         set_docfield_values([{<<"subcategory">>, <<"openboolean">>},
                                              {<<"min">>, null},
                                              {<<"max">>, null},
                                              {<<"value">>, true}]))
    end, true)),
  
  % If the subcategory changes in the field, those changes 
  % should be reflected in the docfield's values. Null openoolean to text test.
  ?_assertEqual(
    v(fun () ->
      set_docfield_values([{<<"subcategory">>, <<"text">>},
                           {<<"min">>, null},
                           {<<"max">>, null},
                           {<<"value">>, null}])
    end, true),
    v(fun () ->
      field:merge_update(set_field_values([{<<"subcategory">>, <<"text">>},
                                           {<<"min">>, null},
                                           {<<"max">>, null}]),
                         set_docfield_values([{<<"subcategory">>, <<"openboolean">>},
                                              {<<"min">>, null},
                                              {<<"max">>, null},
                                              {<<"value">>, null}]))
    end, true)),
  
  % If the subcategory changes in the field, those changes 
  % should be reflected in the docfield's values. Boolean to text test.
  ?_assertEqual(
    v(fun () ->
      set_docfield_values([{<<"subcategory">>, <<"text">>},
                           {<<"min">>, null},
                           {<<"max">>, null},
                           {<<"value">>, <<"false">>}])
    end, true),
    v(fun () ->
      field:merge_update(set_field_values([{<<"subcategory">>, <<"text">>},
                                           {<<"min">>, null},
                                           {<<"max">>, null}]),
                         set_docfield_values([{<<"subcategory">>, <<"boolean">>},
                                              {<<"min">>, null},
                                              {<<"max">>, null},
                                              {<<"value">>, false}]))
    end, true)),
  
  % If the subcategory changes in the field, those changes 
  % should be reflected in the docfield's values. Multiselect to text test.
  ?_assertEqual(
    v(fun () ->
      set_docfield_values([{<<"subcategory">>, <<"text">>},
                           {<<"min">>, null},
                           {<<"max">>, null},
                           {<<"value">>, null}])
    end, true),
    v(fun () ->
      field:merge_update(set_field_values([{<<"subcategory">>, <<"text">>},
                                           {<<"min">>, null},
                                           {<<"max">>, null}]),
                         set_docfield_values([{<<"subcategory">>, <<"multiselect">>},
                                              {<<"min">>, null},
                                              {<<"max">>, null},
                                              {<<"value">>, [<<"false">>, <<"true">>]}]))
    end, true)),
  
  % If the subcategory changes in the field, those changes 
  % should be reflected in the docfield's values. Multiselect to text test.
  ?_assertEqual(
    v(fun () ->
      set_docfield_values([{<<"subcategory">>, <<"text">>},
                           {<<"min">>, null},
                           {<<"max">>, null},
                           {<<"value">>, null}])
    end, true),
    v(fun () ->
      field:merge_update(set_field_values([{<<"subcategory">>, <<"text">>},
                                           {<<"min">>, null},
                                           {<<"max">>, null}]),
                         set_docfield_values([{<<"subcategory">>, <<"multiselect">>},
                                              {<<"min">>, null},
                                              {<<"max">>, null},
                                              {<<"value">>, [<<"false">>, <<"true">>]}]))
    end, true)),
  
  % If the subcategory changes in the field, those changes 
  % should be reflected in the docfield's values. Multiselect to docmultiselect test.
  ?_assertEqual(
    v(fun () ->
      set_docfield_values([{<<"subcategory">>, <<"docmultiselect">>},
                           {<<"min">>, null},
                           {<<"max">>, null},
                           {<<"value">>, [<<"false">>, <<"true">>]}])
    end, true),
    v(fun () ->
      field:merge_update(set_field_values([{<<"subcategory">>, <<"docmultiselect">>},
                                           {<<"min">>, null},
                                           {<<"max">>, null}]),
                         set_docfield_values([{<<"subcategory">>, <<"multiselect">>},
                                              {<<"min">>, null},
                                              {<<"max">>, null},
                                              {<<"value">>, [<<"false">>, <<"true">>]}]))
    end, true)),
  
  % If the subcategory changes in the field, those changes 
  % should be reflected in the docfield's values. Multiselect with default.
  ?_assertEqual(
    v(fun () ->
      set_docfield_values([{<<"subcategory">>, <<"text">>},
                           {<<"min">>, null},
                           {<<"max">>, null},
                           {<<"value">>, <<"false">>}])
    end, true),
    v(fun () ->
      field:merge_update(set_field_values([{<<"subcategory">>, <<"text">>},
                                           {<<"min">>, null},
                                           {<<"max">>, null},
                                           {<<"default">>, <<"false">>}]),
                         set_docfield_values([{<<"subcategory">>, <<"multiselect">>},
                                              {<<"min">>, null},
                                              {<<"max">>, null},
                                              {<<"value">>, [<<"false">>, <<"true">>]}]))
    end, true)),
  
  % If the subcategory changes in the field, those changes 
  % should be reflected in the docfield's values. Date to text test.
  ?_assertEqual(
    v(fun () ->
      field:from_json(doc, jsn:set_value(<<"subcategory">>, <<"text">>, json_docfield()))
    end, true),
    v(fun () ->
      field:merge_update(set_field_values([{<<"subcategory">>, <<"text">>},
                                           {<<"min">>, <<"2001-09-27">>},
                                           {<<"max">>, <<"2010-08-15">>}]), docfield())
    end, true))
].
