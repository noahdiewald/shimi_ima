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

trans_test_() ->
[
  ?_assertEqual(
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d904780e') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value == '99'))", 
    conditions:trans(?EQUAL)),
  
  ?_assertEqual(
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d904780e') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value > '99'))", 
    conditions:trans(?GREATER)),
  
  ?_assertEqual(
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d904780e') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value < '99'))", 
    conditions:trans(?LESS)),
  
  ?_assertEqual(
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d904780e') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "((/99/).test(value)))", 
    conditions:trans(?MATCH)),
  
  ?_assertEqual(
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d904780e') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value == '99')) && " ++
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d9039eed') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d9058ce0') && " ++
    "(value == '888'))", 
    conditions:trans(?DOUBLE)),
  
  ?_assertEqual(
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d904780e') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value == '99')) || " ++
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d9039eed') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d9058ce0') && " ++
    "(value == '888'))", 
    conditions:trans(?WITH_OR)),
  
  ?_assertEqual(
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d904780e') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "!(value == '99'))", 
    conditions:trans(?NEGATIVE)),
  
  ?_assertEqual(
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d904780e') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value == '99')) || " ++
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d9039eed') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d9058ce0') && " ++
    "!(value == '888'))", 
    conditions:trans(?COMPLEX1)),
  
  ?_assertEqual(
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d904780e') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "(value == '99')) && " ++
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d904780e') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "!((/99/).test(value))) || " ++
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d904780e') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d90486fd') && " ++
    "!(value > '99')) || " ++
    "((fieldsetID == 'd5331cbb4d62fe3d2899f142d9039eed') && " ++
    "(fieldId == 'd5331cbb4d62fe3d2899f142d9058ce0') && " ++
    "!(value == '888'))", 
    conditions:trans(?COMPLEX2))
].