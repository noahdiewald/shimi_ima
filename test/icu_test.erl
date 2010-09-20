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
  ?_assertEqual(icu:get_sort_key(<<"h">>), icu:get_sort_key(<<"h">>))
].
