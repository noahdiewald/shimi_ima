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
   ?_assertEqual(js_date:convert("Tue Nov 08 2011 21:24:04 GMT-0600 (CST)"), {{2011,11,8},{21,24,04}}),
   ?_assertEqual(js_date:convert("Te Nov 08 2011 21:24:04 GMT-0600 (CST)"), bad_date)
 ].

to_string_test_() ->
 [
   ?_assertEqual(js_date:to_string({{2011,11,8},{21,24,04}}), "Tue, 08 Nov 2011 21:24:04 GMT")
 ].
