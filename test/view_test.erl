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

make_wrq(Method, RawPath, Headers) ->
    wrq:create(Method, {1,1}, RawPath, mochiweb_headers:from_list(Headers)).

fake_reqdata() ->
     make_wrq('GET', "http://admin:dogoats@127.0.0.1:5984" ++
                  "/project-7e8b897216ff005928f59c5af54972da/" ++ 
                  "_all_docs?startkey=\"_design/\"&endkey=\"_design0\"", []).

creation_test_() ->
    {"Create a vq record",
     [
      {"New blank",
       ?_assertEqual(#vq{}, view:new())},
      {"From reqdata",
       ?_assertEqual(#vq{startkey = <<"_design/">>, endkey = <<"_design0">>}, 
                     view:from_reqdata(fake_reqdata()))},
      {"From list with list JSON values",
       ?_assertEqual(#vq{startkey = <<"_design/">>, endkey = <<"_design0">>},
                     view:from_list([{"startkey", "\"_design/\""}, 
                                     {"endkey", "\"_design0\""}]))},
      {"From list with decoded JSON values",
       ?_assertEqual(#vq{startkey = <<"_design/">>, endkey = <<"_design0">>},
                     view:from_list([{"startkey", <<"_design/">>}, 
                                     {"endkey", <<"_design0">>}]))}
     ]}.
