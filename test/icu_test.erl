%%% Copyright 2011 Noah Diewald <noah@diewald.me>
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
%%% @doc This module contains unit tests for icu.

include_lib("eunit/include/eunit.hrl").

define(COMPLEX_RULES, "\"&9<a,A<\\u0101,a\\u0304,\\u0100,A\\u0304<c,C<ae,AE<a\\u035ee,A\\u035eE<e,E<\\u0113,e\\u0304,\\u0112,E\\u0304<h,H<i,I<\\u012b,i\\u0304,\\u012a,I\\u0304<k,K<m,M<n,N<o,O<\\u014D,o\\u0304,\\u014C,O\\u0304<p,P<q,Q<r,R<s,S<t,T<u,U<\\u016B,u\\u0304,\\u016A,U\\u0304<w,W<y,Y\"").
    
unescape_test_() ->
[
  % Basic case to esure that things aren't very broken
  ?_assertEqual(<<104>>, jsn:decode("\"h\"")),
  
  % Unescaped values should be correct hā
  ?_assertEqual(<<104,196,129>>, jsn:decode("\"h\\u0101\"")),
  
  % Unescaped values should be correct for ā
  ?_assertEqual(<<196,129>>, jsn:decode("\"\\u0101\"")),
  
  % Unescaped values should be correct for COMPLEX_RULES
  ?_assertEqual(<<38,57,60,97,44,65,60,196,129,44,97,204,132,44,196,128,44,65,204,132,60,99,44,67,60,97,101,44,65,69,60,97,205,158,101,44,65,205,158,69,60,101,44,69,60,196,147,44,101,204,132,44,196,146,44,69,204,132,60,104,44,72,60,105,44,73,60,196,171,44,105,204,132,44,196,170,44,73,204,132,60,107,44,75,60,109,44,77,60,110,44,78,60,111,44,79,60,197,141,44,111,204,132,44,197,140,44,79,204,132,60,112,44,80,60,113,44,81,60,114,44,82,60,115,44,83,60,116,44,84,60,117,44,85,60,197,171,44,117,204,132,44,197,170,44,85,204,132,60,119,44,87,60,121,44,89>>, jsn:decode(?COMPLEX_RULES))
  
].

get_sort_key_test_() ->
[
  % Ensure that a properly formed tuple is returned
  ?_assertMatch({ok, _}, icu:get_sort_key(<<"hey">>)),
  
  % Equivalent binaries should get equivalent keys
  ?_assertEqual(icu:get_sort_key(<<"h">>), icu:get_sort_key(<<"h">>)),
  
  % String or key arg are the same
  ?_assertEqual(icu:get_sort_key(<<"hey">>), icu:get_sort_key("hey")),
  
  % TODO: error on bad locale
  % Detect bad argument type
  %?_assertError(badarg, icu:get_sort_key(locale, "ha͞ew", "hey")),
  
  % Ensure that a locale works
  ?_assertMatch({ok, _}, icu:get_sort_key(locale, "en_US", <<"hey">>)),
  
  % Equivalent binaries should get equivalent keys with locales too.
  ?_assertEqual(icu:get_sort_key(locale, "en_US", <<"h">>), icu:get_sort_key(locale, "en_US", <<"h">>)),
  
  % With larger data
  ?_assertEqual(icu:get_sort_key(locale, "en_US", <<"0123456789112345678921234567893123456789412345678951234567895123456789612345678971234567898123456789">>), icu:get_sort_key(locale, "en_US", <<"0123456789112345678921234567893123456789412345678951234567895123456789612345678971234567898123456789">>)),
  
  % The keys must always be the same
  ?_assertEqual({ok, <<58,1,5,1,5,0>>}, icu:get_sort_key(locale, "en_US", <<"h">>)),
  
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
  41,142,1,69,40,1,48,48,18,0>>}, icu:get_sort_key(locale, "en_US", <<"0123456789112345678921234567893123456789412345678951234567895123456789612345678971234567898123456789">>)),
  
  % Blank locale is ok
  ?_assertMatch({ok, _}, icu:get_sort_key(locale, "", <<"h">>))
].