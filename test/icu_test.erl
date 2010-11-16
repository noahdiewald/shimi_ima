-module(icu_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(COMPLEX_RULES, "&9<a,A<\\u0101,a\\u0304,\\u0100,A\\u0304<c,C<ae,AE<a\\u035ee,A\\u035eE<e,E<\\u0113,e\\u0304,\\u0112,E\\u0304<h,H<i,I<\\u012b,i\\u0304,\\u012a,I\\u0304<k,K<m,M<n,N<o,O<\\u014D,o\\u0304,\\u014C,O\\u0304<p,P<q,Q<r,R<s,S<t,T<u,U<\\u016B,u\\u0304,\\u016A,U\\u0304<w,W<y,Y").

get_sort_key_test_() ->
[
  % Detect bad arg
  ?_assertError(badarg, icu:get_sort_key("hey")),
  
  % Ensure that a properly formed tuple is returned
  ?_assertMatch({ok, _}, icu:get_sort_key(<<"hey">>)),
  
  % Equivalent binaries should get equivalent keys
  ?_assertEqual(icu:get_sort_key(<<"h">>), icu:get_sort_key(<<"h">>)),
  
  % Detect bad argument type
  ?_assertError(badarg, icu:get_sort_key("ha͞ew", "hey")),
  
  % Ensure that a locale works
  ?_assertMatch({ok, _}, icu:get_sort_key("en_US", <<"hey">>)),
  
  % Equivalent binaries should get equivalent keys with locales too.
  ?_assertEqual(icu:get_sort_key("en_US", <<"h">>), icu:get_sort_key("en_US", <<"h">>)),
  
  % With larger data
  ?_assertEqual(icu:get_sort_key("en_US", <<"0123456789112345678921234567893123456789412345678951234567895123456789612345678971234567898123456789">>), icu:get_sort_key("en_US", <<"0123456789112345678921234567893123456789412345678951234567895123456789612345678971234567898123456789">>)),
  
  % The keys must always be the same
  ?_assertEqual({ok, <<58,1,5,1,5,0>>}, icu:get_sort_key("en_US", <<"h">>)),
  
  % With larger data
  ?_assertEqual({ok, 
<<41,124,41,126,41,128,41,130,41,132,41,134,41,136,41,138,41,140,41,142,41,126,
  41,126,41,128,41,130,41,132,41,134,41,136,41,138,41,140,41,142,41,128,41,126,
  41,128,41,130,41,132,41,134,41,136,41,138,41,140,41,142,41,130,41,126,41,128,
  41,130,41,132,41,134,41,136,41,138,41,140,41,142,41,132,41,126,41,128,41,130,
  41,132,41,134,41,136,41,138,41,140,41,142,41,134,41,126,41,128,41,130,41,132,
  41,134,41,136,41,138,41,140,41,142,41,134,41,126,41,128,41,130,41,132,41,134,
  41,136,41,138,41,140,41,142,41,136,41,126,41,128,41,130,41,132,41,134,41,136,
  41,138,41,140,41,142,41,138,41,126,41,128,41,130,41,132,41,134,41,136,41,138,
  41,140,41,142,41,140,41,126,41,128,41,130,41,132,41,134,41,136,41,138,41,140,
  41,142,1,69,40,1,48,48,18,0>>}, icu:get_sort_key("en_US", <<"0123456789112345678921234567893123456789412345678951234567895123456789612345678971234567898123456789">>)),
  
  % Blank locale is ok
  ?_assertMatch({ok, _}, icu:get_sort_key("", <<"h">>))
].
