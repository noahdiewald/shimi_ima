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
%%% @doc Tests for view functions

-module(view_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("types.hrl").

creation_test_() ->
    {"Create a vq record",
     [
      {"New blank",
       ?_assertEqual(#vq{}, view:new())},
      {"From list with list JSON values",
       ?_assertEqual(#vq{startkey = <<"_design/">>, endkey = <<"_design0">>},
                     view:from_list([{<<"startkey">>, <<"\"_design/\"">>}, 
                                     {<<"endkey">>, <<"\"_design0\"">>}]))}
     ]}.
