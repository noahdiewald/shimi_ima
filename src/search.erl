%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of dictionary_maker.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
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
         values/4,
         values/6
        ]).

-include_lib("webmachine/include/webmachine.hrl").

values(_Index, [], _R, _S) ->
    [{<<"rows">>, false}];
values(Index, Query, R, S) ->
    {RE, Rows, Json} = get_filter_args("index", Query, Index, R, S),
    prep_ret(i_filter(Rows, RE, []), Json).
    
values(_Doctype, [], _Fields, _Exclude, _R, _S) ->
    [{<<"rows">>, false}];
values(Doctype, Query, [], false, R, S) ->
    {RE, Rows, Json} = get_filter_args("all_vals", Query, Doctype, R, S),
    prep_ret(filter(Rows, RE, []), Json);
values(_Doctype, Query, [Field|[]], false, R, S) ->
    values(binary_to_list(Field), Query, [], false, R, S);
values(Doctype, Query, Fields=[_,_|_], false, R, S) ->
    {RE, Rows, Json} = get_filter_args("all_vals", Query, Doctype, R, S),
    prep_ret(filter(Rows, RE, Fields, true, []), Json);
values(Doctype, Query, Fields, true, R, S) ->
    {RE, Rows, Json} = get_filter_args("all_vals", Query, Doctype, R, S),
    prep_ret(filter(Rows, RE, Fields, false, []), Json).

prep_ret(Rows, Json) ->
    jsn:set_value(<<"rows">>, Rows, Json).

get_filter_args(View, Query, Design, R, S) ->
    {ok, RE} = re:compile(list_to_binary(Query), [unicode]),
    {ok, Json} = couch:get_view_json(Design, View, R, S),
    Rows = jsn:get_value(<<"rows">>, Json),
    {RE, Rows, Json}.

%% @doc filter for searches on indexes
i_filter([], _Query, Acc) ->
    lists:reverse(Acc);
i_filter([Row|T], Query, Acc) ->
    case i_filter2(jsn:get_value(<<"key">>, Row), Query) of
        false -> i_filter(T, Query, Acc);
        _ -> i_filter(T, Query, [Row|Acc])
    end.

i_filter2([], _Query) ->
    false;
i_filter2([[_,K]|T], Query) ->
    case re:run(K, Query) of
        nomatch -> i_filter2(T, Query);
        _ -> true
    end.

%% @doc simple filter
filter([], _Query, Acc) ->
    lists:reverse(Acc);
filter([Row|T], Query, Acc) ->
    [_, K] = jsn:get_value(<<"key">>, Row),
    case re:run(K, Query) of
        nomatch -> filter(T, Query, Acc);
        _ -> filter(T, Query, [Row|Acc])
    end.

%% @doc filter for included/excluded fields
filter([], _Query, _Fields, _Answer, Acc) ->
    lists:reverse(Acc);
filter([Row|T], Query, Fields, Answer, Acc) ->
    [_, K] = jsn:get_value(<<"key">>, Row),
    V = jsn:get_value(<<"value">>, Row),
    F = fun () -> filter(T,  Query, Fields, Answer, Acc) end,
    case {re:run(K, Query), lists:member(V, Fields)} of
        {nomatch, _} -> F();
        {_, Answer} ->
            filter(T, Query, Fields, Answer, [Row|Acc]);
        _ -> F()
    end.
