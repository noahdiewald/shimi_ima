%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 University of Wisconsin Madison Board of Regents.
%% Copyright (c) 2010 University of Wisconsin Madison Board of Regents
%%
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
%% THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(conditions_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(EQUAL,
  [[{<<"is_or">>,false},{<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"equal">>},
    {<<"argument">>,<<"99">>}]]
).

-define(GREATER,
  [[{<<"is_or">>,false},{<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"greater">>},
    {<<"argument">>,<<"99">>}]]
).

-define(LESS,
  [[{<<"is_or">>,false},{<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"less">>},
    {<<"argument">>,<<"99">>}]]
).

-define(MATCH,
  [[{<<"is_or">>,false},{<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"match">>},
    {<<"argument">>,<<"99">>}]]
).

-define(DOUBLE,
  [[{<<"is_or">>,false},{<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"equal">>},
    {<<"argument">>,<<"99">>}],
   [{<<"is_or">>,false},
    {<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d9058ce0">>},
    {<<"operator">>,<<"equal">>},
    {<<"argument">>,<<"888">>}]]
).

-define(WITH_OR,
  [[{<<"is_or">>,false},{<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"equal">>},
    {<<"argument">>,<<"99">>}],
   [{<<"is_or">>,true},{<<"negate">>,false}],
   [{<<"is_or">>,false},
    {<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d9058ce0">>},
    {<<"operator">>,<<"equal">>},
    {<<"argument">>,<<"888">>}]]
).

-define(NEGATIVE,
  [[{<<"is_or">>,false},{<<"negate">>,true},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"equal">>},
    {<<"argument">>,<<"99">>}]]
).

-define(COMPLEX1,
  [[{<<"is_or">>,false},{<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"equal">>},
    {<<"argument">>,<<"99">>}],
   [{<<"is_or">>,true},{<<"negate">>,false}],
   [{<<"is_or">>,false},
    {<<"negate">>,true},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d9058ce0">>},
    {<<"operator">>,<<"equal">>},
    {<<"argument">>,<<"888">>}]]
).

-define(COMPLEX2,
  [[{<<"is_or">>,false},{<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"equal">>},
    {<<"argument">>,<<"99">>}],
   [{<<"is_or">>,false},{<<"negate">>,true},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"match">>},
    {<<"argument">>,<<"99">>}],
   [{<<"is_or">>,true},{<<"negate">>,false}],
   [{<<"is_or">>,false},{<<"negate">>,true},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"greater">>},
    {<<"argument">>,<<"99">>}],
   [{<<"is_or">>,true},{<<"negate">>,false}],
   [{<<"is_or">>,false},
    {<<"negate">>,true},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d9058ce0">>},
    {<<"operator">>,<<"equal">>},
    {<<"argument">>,<<"888">>}]]
).

-define(ACCENT,
  [[{<<"is_or">>,false},{<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"match">>},
    {<<"argument">>,<<"9é9">>}]]
).

-define(SLASH,
  [[{<<"is_or">>,false},{<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"match">>},
    {<<"argument">>,<<"9\\9">>}]]
).

-define(ESCAPE,
  [[{<<"is_or">>,false},{<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"match">>},
    {<<"argument">>,<<"9\\w9">>}]]
).

-define(QUOTE,
  [[{<<"is_or">>,false},{<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"match">>},
    {<<"argument">>,<<"9'9">>}]]
).

-define(DOUBLE_QUOTE,
  [[{<<"is_or">>,false},{<<"negate">>,false},
    {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
    {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
    {<<"operator">>,<<"match">>},
    {<<"argument">>,<<"9\"9">>}]]
).

escape_test() ->
[
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "((/9\\\\9/).test(value)))", 
    conditions:trans(?SLASH)),
  
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "((/9é9/).test(value)))", 
    conditions:trans(?ACCENT)),
  
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "((/9\\\\w9/).test(value)))", 
    conditions:trans(?ESCAPE)),
  
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "((/9\\'9/).test(value)))", 
    conditions:trans(?QUOTE)),
  
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "((/9\\\"9/).test(value)))", 
    conditions:trans(?DOUBLE_QUOTE))
].

trans_test_() ->
[
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value == '99'))", 
    conditions:trans(?EQUAL)),
  
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value > '99'))", 
    conditions:trans(?GREATER)),
  
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value < '99'))", 
    conditions:trans(?LESS)),
  
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "((/99/).test(value)))", 
    conditions:trans(?MATCH)),
  
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value == '99')) && " ++
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d9058ce0') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d9058ce0') && " ++
    "(value == '888'))", 
    conditions:trans(?DOUBLE)),
  
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value == '99')) || " ++
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d9058ce0') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d9058ce0') && " ++
    "(value == '888'))", 
    conditions:trans(?WITH_OR)),
  
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "!(value == '99'))", 
    conditions:trans(?NEGATIVE)),
  
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value == '99')) || " ++
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d9058ce0') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d9058ce0') && " ++
    "!(value == '888'))", 
    conditions:trans(?COMPLEX1)),
  
  ?_assertEqual(
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value == '99')) && " ++
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "!((/99/).test(value))) || " ++
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "!(value > '99')) || " ++
    "(!(fieldId === 'd5331cbb4d62fe3d2899f142d9058ce0') || " ++
    "(fieldId === 'd5331cbb4d62fe3d2899f142d9058ce0') && " ++
    "!(value == '888'))", 
    conditions:trans(?COMPLEX2))
].