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
%%% @doc For testing query condition building

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

-define(EQUAL_INT,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,99}]]
       ).

-define(EQUAL_FLOAT,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,99.9}]]
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

-define(MEMBER,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"member">>},
          {<<"argument">>,<<"99">>}]]
       ).

-define(BLANK,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"blank">>},
          {<<"argument">>,<<"">>}]]
       ).

-define(TRUE,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"true">>},
          {<<"argument">>,<<"">>}]]
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

expected_test_() ->
    [
     ?_assertEqual(
        "{'d5331cbb4d62fe3d2899f142d90486fd': false, " ++
            "'allVisited': function () " ++
            "{return (this['d5331cbb4d62fe3d2899f142d90486fd'])}}",
        conditions:expect(?EQUAL)),
    
     ?_assertEqual(
        "{'d5331cbb4d62fe3d2899f142d9058ce0': false, " ++
            "'d5331cbb4d62fe3d2899f142d90486fd': false, "
        "'allVisited': function () " ++
            "{return (this['d5331cbb4d62fe3d2899f142d9058ce0'] && " ++
            "this['d5331cbb4d62fe3d2899f142d90486fd'])}}",
        conditions:expect(?COMPLEX1))
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
            "(value == 99))", 
        conditions:trans(?EQUAL_INT)),
     
     ?_assertEqual(
        "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
            "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
            "(value == 99.9))", 
        conditions:trans(?EQUAL_FLOAT)),
    
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
            "(value.isBlank()))", 
        conditions:trans(?BLANK)),
  
     ?_assertEqual(
        "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
            "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
            "(value.indexOf('99') > -1))",
        conditions:trans(?MEMBER)),
  
     ?_assertEqual(
        "(!(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') || " ++
            "(fieldId === 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
            "(value == true))", 
        conditions:trans(?TRUE)),
  
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
