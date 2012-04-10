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

%%% @copyright 2012 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc Functions to filter view output used for searching.

-module(search).

-export([
         values/4
        ]).

-include_lib("webmachine/include/webmachine.hrl").

values(Doctype, Query, R, S) ->
    {ok, RE} = re:compile(list_to_binary(Query), [unicode]),
    {ok, Json} = couch:get_view_json(Doctype, "all_vals", R, S),
    Rows = jsn:get_value(<<"rows">>, Json),
    Rows2 = filter(Rows, RE, []),
    jsn:set_value(<<"rows">>, Rows2, Json).

filter([], _Query, Acc) ->
    lists:reverse(Acc);
filter([Row|T], Query, Acc) ->
    [_, K] = jsn:get_value(<<"key">>, Row),
    case re:run(K, Query) of
        nomatch -> filter(T, Query, Acc);
        _ -> filter(T, Query, [Row|Acc])
    end.
