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
%%% @doc This module contains unit tests for icu.

-module(icu_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(TWO_RULES, "\"&e < é <<< É &h < ''\"").

-define(COMPLEX_RULES, "\"&9<a,A<\\u0101,\\u0100<c,C<ae,AE<a\\u035ee,A\\u035eE<e,E<\\u0113,\\u0112<h,H<i,I<\\u012b,\\u012a<k,K<m,M<n,N<o,O<\\u014D,\\u014C<p,P<q,Q<r,R<s,S<t,T<u,U<\\u016B,\\u016A<w,W<y,Y\"").

-define(POTAWATOMI_RULES, "\"&9<a,A,'n#a','n#A'<b,B,'n#b','n#B'<ch,Ch,CH,'n#ch','n#Ch','n#CH'<d,D,'n#d','n#D'<e,E,'n#e','n#E'<é,É,'n#é','n#É'<g,G,'n#g','n#G'<h,H,'n#h','n#H'<'','n#'''<i,I,'n#i','n#I'<j,J,'n#j','n#J'<k,K,'n#k','n#K'<kw,Kw,KW,'n#kw','n#Kw','n#KW'<m,M,'n#m','n#M'<n,N,'n#n','n#N'<o,O,'n#o','n#O'<p,P,'n#p','n#P'<s,S,'n#s','n#S'<sh,Sh,SH,'n#sh','n#Sh','n#SH'<t,T,'n#t','n#T'<u,U,'n#u','n#U'<w,W,'n#w','n#W'<y,Y,'n#y','n#Y'<z,Z,'n#z','n#Z'<zh,Zh,ZH,'n#zh','n#Zh','n#ZH'\"").

-define(QUOTE_RULES, "\"&9<a,'n#a'<'','n#'''\"").

-define(SIMPLE_RULES, "\"&9<e,E<a,A\"").

trules() ->
    ustring:new(jsn:decode(?TWO_RULES), utf8).

qrules() ->
  ustring:new(jsn:decode(?QUOTE_RULES), utf8).

potawatomi() ->
  ustring:new(jsn:decode(?POTAWATOMI_RULES), utf8).

complex() ->
  ustring:new(jsn:decode(?COMPLEX_RULES), utf8).

simple() ->
  ustring:new(jsn:decode(?SIMPLE_RULES), utf8).
  
unescape_test_() ->
[
  % Basic case to esure that things aren't very broken
  ?_assertEqual(<<104>>, jsn:decode("\"h\"")),
  
  % Unescaped values should be correct hā
  ?_assertEqual(<<104,196,129>>, jsn:decode("\"h\\u0101\"")),
  
  % Unescaped values should be correct for ā
  ?_assertEqual(<<196,129>>, jsn:decode("\"\\u0101\"")),
  
  % Unescaped values should be correct for COMPLEX_RULES
  ?_assertEqual(<<38,57,60,97,44,65,60,196,129,44,196,128,60,99,44,67,60,97,101,44,65,69,60,97,
  205,158,101,44,65,205,158,69,60,101,44,69,60,196,147,44,196,146,60,104,44,72,
  60,105,44,73,60,196,171,44,196,170,60,107,44,75,60,109,44,77,60,110,44,78,60,
  111,44,79,60,197,141,44,197,140,60,112,44,80,60,113,44,81,60,114,44,82,60,
  115,44,83,60,116,44,84,60,117,44,85,60,197,171,44,197,170,60,119,44,87,60,
  121,44,89>>, jsn:decode(?COMPLEX_RULES))
].

character_normalization_test_() ->
[
  % The combining a-macron character and the non-combining a-macron are the same
  ?_assertEqual(unicode:characters_to_list(ustring:new(<<97,204,132>>, utf8), {utf16, little}), 
                unicode:characters_to_list(<<196,129>>, utf8))
].

rule_sortkey_test_() ->
[
  % Ensure that a properly formed tuple is returned
  ?_assertMatch({ok, _}, icu:sortkey(simple(), ustring:new(<<"hey">>, latin1))),
  
  % Ensure that a properly formed tuple is returned
  ?_assertMatch({ok, _}, icu:sortkey(complex(), ustring:new(<<"hey">>, latin1))),
  
  % Ensure that a normal sort key does what is expected
  ?_assert(icu:sortkey("", ustring:new(<<"hay">>, latin1)) < icu:sortkey("", ustring:new(<<"hey">>, latin1))),
  
  % Ensure that a tailored sort key does the opposite
  ?_assert(icu:sortkey(simple(), ustring:new(<<"hay">>, latin1)) > icu:sortkey(simple(), ustring:new(<<"hey">>, latin1))),
  
  % A series of tests on capitalization in minimal pairs
  ?_assert(icu:sortkey(complex(), ustring:new(<<"ha͞ew">>, latin1)) < icu:sortkey(complex(), ustring:new(<<"hA͞Ew">>, latin1))),
  
  ?_assert(icu:sortkey(complex(), ustring:new(<<"ha͞ew">>, latin1)) > icu:sortkey(complex(), ustring:new(<<"hA͞Ea">>, latin1))),
  
  ?_assert(icu:sortkey(complex(), ustring:new(<<196,129>>, utf8)) < icu:sortkey(complex(), ustring:new(<<196,128>>, utf8))),
  
  % Ignore the 'n#', except in minimal pairs
  ?_assert(icu:sortkey(potawatomi(), ustring:new(jsn:decode("\"n#ab\""), utf8)) < icu:sortkey(potawatomi(), ustring:new(jsn:decode("\"bc\""), utf8))),
  
  ?_assert(icu:sortkey(potawatomi(), ustring:new(jsn:decode("\"n#a\""), utf8)) > icu:sortkey(potawatomi(), ustring:new(jsn:decode("\"a\""), utf8))),
  
  % Single quote follows 'a'
  ?_assert(icu:sortkey(qrules(), ustring:new(jsn:decode("\"'\""), utf8)) > icu:sortkey(qrules(), ustring:new(jsn:decode("\"a\""), utf8))),
  
  ?_assert(icu:sortkey(qrules(), ustring:new(jsn:decode("\"''\""), utf8)) > icu:sortkey(qrules(), ustring:new(jsn:decode("\"aa\""), utf8))),
  
  ?_assert(icu:sortkey(qrules(), ustring:new(jsn:decode("\"'b\""), utf8)) > icu:sortkey(qrules(), ustring:new(jsn:decode("\"ab\""), utf8))),
  
  ?_assert(icu:sortkey(qrules(), ustring:new(jsn:decode("\"b'b\""), utf8)) > icu:sortkey(qrules(), ustring:new(jsn:decode("\"bab\""), utf8))),

 % Two rules
 ?_assert(icu:sortkey(trules(), ustring:new(jsn:decode("\"H\""), utf8)) < icu:sortkey(trules(), ustring:new(jsn:decode("\"'\""), utf8))),

 ?_assert(icu:sortkey(trules(), ustring:new(jsn:decode("\"hb\""), utf8)) < icu:sortkey(trules(), ustring:new(jsn:decode("\"'a\""), utf8))),

 ?_assert(icu:sortkey(trules(), ustring:new(jsn:decode("\"eb\""), utf8)) < icu:sortkey(trules(), ustring:new(jsn:decode("\"Éa\""), utf8))),

  % Various facts about Potawatomi
  ?_assert(icu:sortkey(potawatomi(), ustring:new(jsn:decode("\"a'p\""), utf8)) > icu:sortkey(potawatomi(), ustring:new(jsn:decode("\"aap\""), utf8))),
  
  ?_assert(icu:sortkey(potawatomi(), ustring:new(jsn:decode("\"n#'p\""), utf8)) > icu:sortkey(potawatomi(), ustring:new(jsn:decode("\"n#ap\""), utf8)))
].

locale_sortkey_test_() ->
[
  % Ensure that a properly formed tuple is returned
  ?_assertMatch({ok, _}, icu:sortkey("en_US", ustring:new(<<"hey">>, latin1))),
  
  % Equivalent binaries should get equivalent keys
  ?_assertEqual(icu:sortkey("en_US", ustring:new(<<"h">>, latin1)), icu:sortkey("en_US", ustring:new(<<"h">>, latin1))),
  
  % UTF-8 and Latin1 are the same
  ?_assertEqual(icu:sortkey("en_US", ustring:new(<<"h">>, latin1)), icu:sortkey("en_US", ustring:new(<<"h">>, utf8))),
  
  % TODO: error on bad locale
  % Detect bad argument type
  %?_assertError(badarg, icu:get_sort_key(locale, "ha͞ew", "hey")),
  
  % With larger data
  ?_assertEqual(icu:sortkey("en_US", ustring:new(<<"0123456789112345678921234567893123456789412345678951234567895123456789612345678971234567898123456789">>, latin1)), icu:sortkey("en_US", ustring:new(<<"0123456789112345678921234567893123456789412345678951234567895123456789612345678971234567898123456789">>, latin1))),
  
  % The keys must always be the same
  ?_assertEqual({ok, <<53,1,5,1,5,0>>}, icu:sortkey("en_US", ustring:new(<<"h">>, latin1))),
  
  % With larger data
  ?_assertEqual({ok, 
<<18,20,22,24,26,28,30,32,34,36,20,20,22,24,26,28,30,32,34,36,22,20,22,24,26,
  28,30,32,34,36,24,20,22,24,26,28,30,32,34,36,26,20,22,24,26,28,30,32,34,36,
  28,20,22,24,26,28,30,32,34,36,28,20,22,24,26,28,30,32,34,36,30,20,22,24,26,
  28,30,32,34,36,32,20,22,24,26,28,30,32,34,36,34,20,22,24,26,28,30,32,34,36,1,
  69,40,1,48,48,18,0>>}, icu:sortkey("en_US", ustring:new(<<"0123456789112345678921234567893123456789412345678951234567895123456789612345678971234567898123456789">>, latin1))),
  
  % Blank locale is ok
  ?_assertMatch({ok, _}, icu:sortkey("", ustring:new(<<"h">>, latin1))),
  
  % Order for case as minimal pair
  ?_assert(icu:sortkey("", ustring:new(<<"H">>, latin1)) > icu:sortkey("", ustring:new(<<"h">>, latin1))),
  
  % Order for case not as minimal pair
  ?_assert(icu:sortkey("", ustring:new(<<"Ha">>, latin1)) < icu:sortkey("", ustring:new(<<"hb">>, latin1))),
  
  % Order for case not as minimal pair
  ?_assert(icu:sortkey("en_US", ustring:new(<<"Ha">>, latin1)) < icu:sortkey("en_US", ustring:new(<<"hb">>, latin1))),
  
  % Order for punctuation as minimal pair
  ?_assert(icu:sortkey("", ustring:new(<<"Ha-">>, latin1)) > icu:sortkey("", ustring:new(<<"Ha">>, latin1)))
].
