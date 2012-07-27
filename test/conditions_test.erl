%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of dictionary_maker.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% dictionary_maker is distributed in the hope that it will be
%%% useful, but WITHOUT ANY WARRANTY; without even the implied
%%% warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
%%% PURPOSE. See the GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with dictionary_maker. If not, see
%%% <http://www.gnu.org/licenses/>.

%%% @copyright 2011 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc For testing query condition building

-module(conditions_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(EQUAL,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,<<"99">>}]]
       ).

-define(EQUAL_INT,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,99}]]
       ).

-define(EQUAL_FLOAT,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,99.9}]]
       ).

-define(GREATER,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"greater">>},
          {<<"argument">>,<<"99">>}]]
       ).

-define(LESS,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"less">>},
          {<<"argument">>,<<"99">>}]]
       ).

-define(MATCH,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"match">>},
          {<<"argument">>,<<"99">>}]]
       ).

-define(MEMBER,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"member">>},
          {<<"argument">>,<<"99">>}]]
       ).

-define(BLANK,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"blank">>},
          {<<"argument">>,<<"">>}]]
       ).

-define(TRUE,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"true">>},
          {<<"argument">>,<<"">>}]]
       ).

-define(HAS_EXACTLY,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"hasExactly">>},
          {<<"argument">>,3}]]
       ).

-define(HAS_GREATER,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"hasGreater">>},
          {<<"argument">>,1}]]
       ).

-define(HAS_LESS,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"hasLess">>},
          {<<"argument">>,2}]]
       ).

-define(IS_DEFINED,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"isDefined">>},
          {<<"argument">>,<<"">>}]]
       ).

-define(DOUBLE,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,<<"99">>}],
         [{<<"is_or">>,false},
          {<<"parens">>,false},
          {<<"negate">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d9058ce0">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,<<"888">>}]]
       ).

-define(WITH_OR,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,<<"99">>}],
         [{<<"is_or">>,true},
          {<<"parens">>,false},
          {<<"negate">>,false}],
         [{<<"is_or">>,false},
          {<<"parens">>,false},
          {<<"negate">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d9058ce0">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,<<"888">>}]]
       ).

-define(NEGATIVE,
        [[{<<"is_or">>,false},{<<"negate">>,true},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,<<"99">>}]]
       ).

-define(COMPLEX1,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,<<"99">>}],
         [{<<"is_or">>,true},
          {<<"parens">>,false},
          {<<"negate">>,false}],
         [{<<"is_or">>,false},
          {<<"parens">>,false},
          {<<"negate">>,true},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d9058ce0">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,<<"888">>}]]
       ).

-define(COMPLEX2,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,<<"99">>}],
         [{<<"is_or">>,false},{<<"negate">>,true},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"match">>},
          {<<"argument">>,<<"99">>}],
         [{<<"is_or">>,true},
          {<<"parens">>,false},
          {<<"negate">>,false}],
         [{<<"is_or">>,false},{<<"negate">>,true},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"greater">>},
          {<<"argument">>,<<"99">>}],
         [{<<"is_or">>,true},
          {<<"parens">>,false},
          {<<"negate">>,false}],
         [{<<"is_or">>,false},
          {<<"parens">>,false},
          {<<"negate">>,true},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d9058ce0">>},
          {<<"operator">>,<<"equal">>},
          {<<"argument">>,<<"888">>}]]
       ).

-define(ACCENT,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"match">>},
          {<<"argument">>,<<"9é9">>}]]
       ).

-define(SLASH,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"match">>},
          {<<"argument">>,<<"9\\9">>}]]
       ).

-define(ESCAPE,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"match">>},
          {<<"argument">>,<<"9\\w9">>}]]
       ).

-define(QUOTE,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"match">>},
          {<<"argument">>,<<"9'9">>}]]
       ).

-define(DOUBLE_QUOTE,
        [[{<<"is_or">>,false},{<<"negate">>,false},
          {<<"parens">>,false},
          {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
          {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
          {<<"operator">>,<<"match">>},
          {<<"argument">>,<<"9\"9">>}]]
       ).
    
escape_test() ->
    [
     ?_assertEqual(<<"(matches('d5331cbb4d62fe3d2899f142d90486fd',/9\\\\9/))">>, 
        conditions:trans(?SLASH)),
  
     ?_assertEqual(<<"(matches('d5331cbb4d62fe3d2899f142d90486fd',/9é9/))">>, 
        conditions:trans(?ACCENT)),
  
     ?_assertEqual(<<"(matches('d5331cbb4d62fe3d2899f142d90486fd',/9\\\\w9/))">>, 
        conditions:trans(?ESCAPE)),
  
     ?_assertEqual(<<"(matches('d5331cbb4d62fe3d2899f142d90486fd',/9\\'9/))">>,
        conditions:trans(?QUOTE)),
  
     ?_assertEqual(<<"(matches('d5331cbb4d62fe3d2899f142d90486fd',/9\\\"9/))">>, 
        conditions:trans(?DOUBLE_QUOTE))
    ].

trans_test_() ->
    [
     ?_assertEqual(<<"(equals('d5331cbb4d62fe3d2899f142d90486fd','99'))">>, 
                   conditions:trans(?EQUAL)),
  
     ?_assertEqual(<<"(equals('d5331cbb4d62fe3d2899f142d90486fd',99))">>, 
                   conditions:trans(?EQUAL_INT)),
     
     ?_assertEqual(<<"(equals('d5331cbb4d62fe3d2899f142d90486fd',99.9))">>, 
                   conditions:trans(?EQUAL_FLOAT)),
    
     ?_assertEqual(<<"(greaterThan('d5331cbb4d62fe3d2899f142d90486fd','99'))">>, 
                   conditions:trans(?GREATER)),
  
     ?_assertEqual(<<"(lessThan('d5331cbb4d62fe3d2899f142d90486fd','99'))">>, 
                   conditions:trans(?LESS)),
  
     ?_assertEqual(<<"(isBlank('d5331cbb4d62fe3d2899f142d90486fd'))">>, 
                   conditions:trans(?BLANK)),
  
     ?_assertEqual(<<"(hasMember('d5331cbb4d62fe3d2899f142d90486fd','99'))">>,
                   conditions:trans(?MEMBER)),
  
     ?_assertEqual(<<"(hasExactly('d5331cbb4d62fe3d2899f142d90486fd',3))">>,
                   conditions:trans(?HAS_EXACTLY)),
  
     ?_assertEqual(<<"(hasGreater('d5331cbb4d62fe3d2899f142d90486fd',1))">>,
                   conditions:trans(?HAS_GREATER)),
  
     ?_assertEqual(<<"(hasLess('d5331cbb4d62fe3d2899f142d90486fd',2))">>,
                   conditions:trans(?HAS_LESS)),
  
     ?_assertEqual(<<"(isDefined('d5331cbb4d62fe3d2899f142d90486fd'))">>,
                   conditions:trans(?IS_DEFINED)),
  
     ?_assertEqual(<<"(isTrue('d5331cbb4d62fe3d2899f142d90486fd'))">>, 
                   conditions:trans(?TRUE)),
  
     ?_assertEqual(<<"(matches('d5331cbb4d62fe3d2899f142d90486fd',/99/))">>, 
                   conditions:trans(?MATCH)),
  
     ?_assertEqual(<<"(equals('d5331cbb4d62fe3d2899f142d90486fd','99')) && (equals('d5331cbb4d62fe3d2899f142d9058ce0','888'))">>, 
                   conditions:trans(?DOUBLE)),
  
     ?_assertEqual(<<"(equals('d5331cbb4d62fe3d2899f142d90486fd','99')) || (equals('d5331cbb4d62fe3d2899f142d9058ce0','888'))">>, 
                   conditions:trans(?WITH_OR)),
  
     ?_assertEqual(<<"(!equals('d5331cbb4d62fe3d2899f142d90486fd','99'))">>, 
                   conditions:trans(?NEGATIVE)),
  
     ?_assertEqual(<<"(equals('d5331cbb4d62fe3d2899f142d90486fd','99')) || (!equals('d5331cbb4d62fe3d2899f142d9058ce0','888'))">>, 
                   conditions:trans(?COMPLEX1)),
  
     ?_assertEqual(<<"(equals('d5331cbb4d62fe3d2899f142d90486fd','99')) && (!matches('d5331cbb4d62fe3d2899f142d90486fd',/99/)) || (!greaterThan('d5331cbb4d62fe3d2899f142d90486fd','99')) || (!equals('d5331cbb4d62fe3d2899f142d9058ce0','888'))">>, 
                   conditions:trans(?COMPLEX2)),
     
     ?_assertEqual(<<"(equals('d5331cbb4d62fe3d2899f142d90486fd','99')) && (!matches('d5331cbb4d62fe3d2899f142d90486fd',/99/)) || ((!greaterThan('d5331cbb4d62fe3d2899f142d90486fd','99')) || (!equals('d5331cbb4d62fe3d2899f142d9058ce0','888')))">>, 
                   conditions:trans(grouped1())),

     ?_assertEqual(<<"(equals('d5331cbb4d62fe3d2899f142d90486fd','99')) && (!matches('d5331cbb4d62fe3d2899f142d90486fd',/99/)) && ((!greaterThan('d5331cbb4d62fe3d2899f142d90486fd','99')) || (!equals('d5331cbb4d62fe3d2899f142d9058ce0','888')))">>, 
                   conditions:trans(grouped2())),

     ?_assertEqual(<<"((equals('created_by_','lindsay')) || (equals('updated_by_','lindsay'))) && ((hasMember('d5331cbb4d62fe3d2899f142d905a4d7','fieldwork')) || (hasMember('4df410fea47fcdc8d509c025929706d3','fieldwork'))) && ((greaterThan('d5331cbb4d62fe3d2899f142d905dafa','2011-06-01')) || (greaterThan('d5331cbb4d62fe3d2899f142d9064c95','2011-06-01')))">>,
                   conditions:trans(complex_parens())),

     ?_assertEqual(<<"((equals('created_by_','lindsay')) || (equals('updated_by_','lindsay'))) && (((hasMember('d5331cbb4d62fe3d2899f142d905a4d7','fieldwork')) || (hasMember('4df410fea47fcdc8d509c025929706d3','fieldwork')))) && ((greaterThan('d5331cbb4d62fe3d2899f142d905dafa','2011-06-01')) || (greaterThan('d5331cbb4d62fe3d2899f142d9064c95','2011-06-01')))">>,
                   conditions:trans(double_parens1())),

     ?_assertEqual(<<"((equals('created_by_','lindsay')) || (equals('updated_by_','lindsay'))) && (((hasMember('d5331cbb4d62fe3d2899f142d905a4d7','fieldwork')) || (hasMember('4df410fea47fcdc8d509c025929706d3','fieldwork')))) && (greaterThan('d5331cbb4d62fe3d2899f142d905dafa','2011-06-01')) || (greaterThan('d5331cbb4d62fe3d2899f142d9064c95','2011-06-01'))">>,
                   conditions:trans(double_parens2())),

     ?_assertEqual(<<"(existentialTest('d5331cbb4d62fe3d2899f142d905a4d7',function (x) {return (hasMember('d5331cbb4d62fe3d2899f142d905a4d7','fieldwork',x)) && (greaterThan('d5331cbb4d62fe3d2899f142d905dafa','2011-06-01',x));}))">>,
                   conditions:trans(simple_existential())),

     ?_assertEqual(<<"(existentialTest('x',function (x) {return (hasMember('x','fieldwork',x)) && (greaterThan('x','2011-06-01',x));})) || (hasMember('x','fieldwork')) && (existentialTest('x',function (x) {return (hasMember('x','fieldwork',x)) || ((hasMember('x','fieldwork',x)) || (hasMember('x','fieldwork',x))) && (greaterThan('x','2011-06-01',x));})) && ((hasMember('x','fieldwork')) || (hasMember('x','fieldwork')))">>,
                   conditions:trans(complex_existential())),

     ?_assertEqual(<<"((existentialTest('x11',function (x) {return (greaterThan('x11','2011-06-01',x)) && (lessThan('x12','2011-09-30',x));})) || (existentialTest('x21',function (x) {return (greaterThan('x21','2011-06-01',x)) && (lessThan('x22','2011-09-30',x));})))">>,
                   conditions:trans(parens_ex_or_ex()))
    ].

% Fixtures

grouped1() ->
    [[{<<"is_or">>,false},{<<"negate">>,false},
      {<<"parens">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
      {<<"operator">>,<<"equal">>},
      {<<"argument">>,<<"99">>}],
     [{<<"is_or">>,false},{<<"negate">>,true},
      {<<"parens">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
      {<<"operator">>,<<"match">>},
      {<<"argument">>,<<"99">>}],
     [{<<"is_or">>,true},
      {<<"parens">>,false},
      {<<"negate">>,false}],
     [{<<"is_or">>,false},{<<"parens">>,<<"open">>},{<<"negate">>,false}],
     [{<<"is_or">>,false},{<<"negate">>,true},
      {<<"parens">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
      {<<"operator">>,<<"greater">>},
      {<<"argument">>,<<"99">>}],
     [{<<"is_or">>,true},
      {<<"parens">>,false},
      {<<"negate">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,true},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d9058ce0">>},
      {<<"operator">>,<<"equal">>},
      {<<"argument">>,<<"888">>}],
    [{<<"is_or">>,false},{<<"parens">>,<<"close">>},{<<"negate">>,false}]].

grouped2() ->
    [[{<<"is_or">>,false},{<<"negate">>,false},
      {<<"parens">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
      {<<"operator">>,<<"equal">>},
      {<<"argument">>,<<"99">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,true},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
      {<<"operator">>,<<"match">>},
      {<<"argument">>,<<"99">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"open">>},{<<"negate">>,false}],
     [{<<"is_or">>,false},{<<"negate">>,true},
      {<<"parens">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d904780e">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d90486fd">>},
      {<<"operator">>,<<"greater">>},
      {<<"argument">>,<<"99">>}],
     [{<<"is_or">>,true},
      {<<"parens">>,false},
      {<<"negate">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,true},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d9058ce0">>},
      {<<"operator">>,<<"equal">>},
      {<<"argument">>,<<"888">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,<<"close">>},
      {<<"negate">>,false}]].

complex_parens() ->
    [[{<<"is_or">>,false},{<<"parens">>,<<"open">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"metadata">>},
      {<<"field">>,<<"created_by_">>},
      {<<"operator">>,<<"equal">>},
      {<<"argument">>,<<"lindsay">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"metadata">>},
      {<<"field">>,<<"updated_by_">>},
      {<<"operator">>,<<"equal">>},
      {<<"argument">>,<<"lindsay">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"close">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"open">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d905a4d7">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d903c781">>},
      {<<"field">>,<<"4df410fea47fcdc8d509c025929706d3">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"close">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"open">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d905dafa">>},
      {<<"operator">>,<<"greater">>},
      {<<"argument">>,<<"2011-06-01">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d903c781">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d9064c95">>},
      {<<"operator">>,<<"greater">>},
      {<<"argument">>,<<"2011-06-01">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"close">>}]].

double_parens1() ->
    [[{<<"is_or">>,false},{<<"parens">>,<<"open">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"metadata">>},
      {<<"field">>,<<"created_by_">>},
      {<<"operator">>,<<"equal">>},
      {<<"argument">>,<<"lindsay">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"metadata">>},
      {<<"field">>,<<"updated_by_">>},
      {<<"operator">>,<<"equal">>},
      {<<"argument">>,<<"lindsay">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"close">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"open">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"open">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d905a4d7">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d903c781">>},
      {<<"field">>,<<"4df410fea47fcdc8d509c025929706d3">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"close">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"close">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"open">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d905dafa">>},
      {<<"operator">>,<<"greater">>},
      {<<"argument">>,<<"2011-06-01">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d903c781">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d9064c95">>},
      {<<"operator">>,<<"greater">>},
      {<<"argument">>,<<"2011-06-01">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"close">>}]].

double_parens2() ->
    [[{<<"is_or">>,false},{<<"parens">>,<<"open">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"metadata">>},
      {<<"field">>,<<"created_by_">>},
      {<<"operator">>,<<"equal">>},
      {<<"argument">>,<<"lindsay">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"metadata">>},
      {<<"field">>,<<"updated_by_">>},
      {<<"operator">>,<<"equal">>},
      {<<"argument">>,<<"lindsay">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"close">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"open">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"open">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d905a4d7">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d903c781">>},
      {<<"field">>,<<"4df410fea47fcdc8d509c025929706d3">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"close">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"close">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d905dafa">>},
      {<<"operator">>,<<"greater">>},
      {<<"argument">>,<<"2011-06-01">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d903c781">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d9064c95">>},
      {<<"operator">>,<<"greater">>},
      {<<"argument">>,<<"2011-06-01">>}]].

complex_existential() ->
    [[{<<"is_or">>,false},{<<"parens">>,<<"exopen">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed1">>},
      {<<"field">>,<<"x">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"x">>},
      {<<"operator">>,<<"greater">>},
      {<<"argument">>,<<"2011-06-01">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"exclose">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"x">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"exopen">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"x">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},{<<"parens">>,<<"open">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"x">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d903c781">>},
      {<<"field">>,<<"x">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"close">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"x">>},
      {<<"operator">>,<<"greater">>},
      {<<"argument">>,<<"2011-06-01">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"exclose">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"open">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"x">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d903c781">>},
      {<<"field">>,<<"x">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"close">>}]].

parens_ex_or_ex() -> 
    [[{<<"is_or">>,false},{<<"parens">>,<<"open">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"exopen">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"x">>},
      {<<"field">>,<<"x11">>},
      {<<"operator">>,<<"greater">>},
      {<<"argument">>,<<"2011-06-01">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"x">>},
      {<<"field">>,<<"x12">>},
      {<<"operator">>,<<"less">>},
      {<<"argument">>,<<"2011-09-30">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"exclose">>}],
     [{<<"is_or">>,true},{<<"parens">>,false}],
     [{<<"is_or">>,false},{<<"parens">>,<<"exopen">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"x">>},
      {<<"field">>,<<"x21">>},
      {<<"operator">>,<<"greater">>},
      {<<"argument">>,<<"2011-06-01">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"x">>},
      {<<"field">>,<<"x22">>},
      {<<"operator">>,<<"less">>},
      {<<"argument">>,<<"2011-09-30">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"exclose">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"close">>}]].

simple_existential() ->
    [[{<<"is_or">>,false},{<<"parens">>,<<"exopen">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d905a4d7">>},
      {<<"operator">>,<<"member">>},
      {<<"argument">>,<<"fieldwork">>}],
     [{<<"is_or">>,false},
      {<<"parens">>,false},
      {<<"negate">>,false},
      {<<"fieldset">>,<<"d5331cbb4d62fe3d2899f142d9039eed">>},
      {<<"field">>,<<"d5331cbb4d62fe3d2899f142d905dafa">>},
      {<<"operator">>,<<"greater">>},
      {<<"argument">>,<<"2011-06-01">>}],
     [{<<"is_or">>,false},{<<"parens">>,<<"exclose">>}]].
