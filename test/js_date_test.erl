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
%%% @doc Tests for fieldset functions

-module(js_date_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/types.hrl").

convert_test_() ->
    [
     ?_assertEqual(js_date:convert("Tue Nov 08 2011 21:24:04 GMT-0600 (CST)"), {ok, {{2011,11,9},{3,24,4}}}),
     ?_assertEqual(js_date:convert("Tue Nov 08 2011 21:24:04 GMT+0600"), {ok, {{2011,11,8},{15,24,4}}}),
     ?_assertEqual(js_date:convert("Tue Nov 08 2011 01:24:04 GMT+0600"), {ok, {{2011,11,7},{19,24,4}}}),
     ?_assertEqual(js_date:convert("Tue Nov 08 2011 01:24:04 GMT-0600 (CST)"), {ok, {{2011,11,8},{7,24,4}}}),
     ?_assertEqual(js_date:convert("Tue Nov 08 2011 01:24:04 GMT-0630"), {ok, {{2011,11,8},{7,54,4}}}),
     ?_assertEqual(js_date:convert("Tue Nov 08 2011 23:33:04 GMT-0030"), {ok, {{2011,11,9},{0,3,4}}}),
     ?_assertEqual(js_date:convert("Te Nov 08 2011 21:24:04 GMT-0600 (CST)"), bad_date)
    ].

to_string_test_() ->
    [
     ?_assertEqual(js_date:to_string({{2011,11,8},{21,24,04}}), "Tue, 08 Nov 2011 21:24:04 GMT")
    ].

both_test_() ->
    [
     ?_assertEqual(js_date:to_string(no_ok(js_date:convert("Tue Nov 08 2011 01:24:04 GMT+0600"))), "Mon, 07 Nov 2011 19:24:04 GMT")
     ].

no_ok({ok, Ok}) ->
    Ok.
