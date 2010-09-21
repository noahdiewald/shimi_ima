-module(icu_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

get_sort_key_test_() ->
[
  % Detect bad arg
  ?_assertError(badarg, icu:get_sort_key("hey")),
  
  % Ensure that a properly formed tuple is returned
  ?_assertMatch({ok, _}, icu:get_sort_key(<<"hey">>)),
  
  % Equivalent binaries should get equivalent keys
  ?_assertEqual(icu:get_sort_key(<<"h">>), icu:get_sort_key(<<"h">>)),
  
  % Detect bad encoding in string
  ?_assertError(badarg, icu:get_sort_key("ha͞ew", "hey")),
  
  % Ensure that a reasonable locale works
  ?_assertMatch({ok, _}, icu:get_sort_key("en_US", <<"hey">>)),
  
  % Equivalent binaries should get equivalent keys
  ?_assertEqual(icu:get_sort_key("en_US", <<"h">>), icu:get_sort_key("en_US", <<"h">>)),
  
  % The keys must always be the same of a locale
  ?_assertEqual({ok, <<58,1,5,1,5,0>>}, icu:get_sort_key("en_US", <<"h">>)),
  
  % Blank is ok
  ?_assertMatch({ok, _}, icu:get_sort_key("", <<"h">>))
].
