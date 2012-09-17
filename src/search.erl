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
-include_lib("types.hrl").

%% @doc Retrieve values for a search over a user defined index.
%% -spec values(string(), string(), utils:reqdata(), any()) -> [{any(),any()}].
values(_Index, [], _R, _S) ->
    [{<<"rows">>, false}];
values(Index, Query, R, S) ->
    {ok, RE} = re:compile(list_to_binary(Query), [unicode]),
    {ok, Json} = couch:get_view_json(Index, "index", R, S),
    Rows = jsn:get_value(<<"rows">>, Json),
    Filtered = i_filter(Rows, RE, []),
    [{<<"index_listing">>, true}, {<<"rows">>, Filtered}, 
     {<<"total_rows">>, length(Filtered)}].

%% @doc Retrieve values for a search over fields found in a a document
%% of a particular type.
-spec values(binary(), string(), [binary()], [binary()], utils:reqdata(), any()) -> [{binary(), list()|boolean()}].
values(_Doctype, [], _Fields, _Exclude, _R, _S) -> % No query
    [{<<"rows">>, false}];
values(Doctype, Query, [], ExFields, R, S) -> % All fields, or exclusive fields
    Fields = get_fields(Doctype, ExFields, R, S),
    values(Doctype, Query, Fields, [], R, S);
values(Doctype, Query, Fields, [], R, S) -> % Inclusive fields
    {ok, RE} = re:compile(list_to_binary(Query), [unicode]),
    TID = ets:new(list_to_atom("search-" ++ utils:uuid()), [public]),
    ets:insert(TID, {total, 0}),
    F = fun(X) ->
                do_search(Doctype, RE, X, TID, R, S)
        end,
    ok = utils:peach(F, Fields, 10),
    Complete = prep_ret(TID),
    ets:delete(TID),
    Complete.

prep_ret(TID) ->
    F = fun(X, Acc) ->
                case X of
                    {total, _} -> Acc;
                    {Id, Field} when is_binary(Id) -> [Field|Acc]
                end
        end,
    [{total, Total}] = ets:lookup(TID, total),
    Rows = ets:foldl(F, [], TID),
    [{<<"total_rows">>, Total}, {<<"rows">>, Rows}].

get_matches(RE, QS, R, S) ->
    {ok, Json} = couch:get_view_json("fields", "search", QS, R, S),
    filter(jsn:get_value(<<"rows">>, Json), RE, []).

%% @doc filter for searches on indexes
i_filter([], _RE, Acc) ->
    lists:reverse(Acc);
i_filter([Row|T], RE, Acc) ->
    case i_filter2(jsn:get_value(<<"key">>, Row), RE) of
        false -> i_filter(T, RE, Acc);
        _ -> i_filter(T, RE, [Row|Acc])
    end.

i_filter2([], _RE) ->
    false;
i_filter2([[_,K]|T], RE) ->
    case re:run(K, RE) of
        nomatch -> i_filter2(T, RE);
        _ -> true
    end.

%% @doc simple filter
filter([], _RE, Acc) ->
    Acc;
filter([H|T], RE, Acc) ->
    [_, _, _, K] = jsn:get_value(<<"key">>, H),
    case re:run(K, RE) of
        nomatch -> filter(T, RE, Acc);
        _ -> filter(T, RE, [jsn:set_value(<<"key">>, K, H)|Acc])
    end.

do_search(Doctype, RE, Field, TID, R, S) ->
    VQ = #vq{startkey = [Doctype, Field, []],
             endkey = [Doctype, Field, <<"">>],
             descending = true},
    Matches = get_matches(RE, view:to_string(VQ), R, S),
    case length(Matches) of
        0 ->
            ok;
        Num ->
            Total = case ets:lookup(TID, total) of
                        [] -> Num;
                        [{total, OldTotal}] -> OldTotal + Num
                    end, 
            ets:insert(TID, [{total, Total},
                             {Field, [{<<"id">>, Field}, 
                                      {<<"total_rows">>, Num}, 
                                      {<<"rows">>, Matches}]}]),
            ok
    end.

%% do_search(_Doctype, _RE, [], Acc, _R, _S) ->
%%     Acc;
%% do_search(Doctype, RE, [H|T], Acc, R, S) ->
%%     VQ = #vq{startkey = [Doctype, H, []],
%%              endkey = [Doctype, H, <<"">>],
%%              descending = true},
%%     Matches = get_matches(RE, view:to_string(VQ), R, S),
%%     case length(Matches) of
%%         0 ->
%%             do_search(Doctype, RE, T, Acc, R, S);
%%         _ ->
%%             do_search(Doctype, RE, T, [[{<<"id">>, H}, 
%%                                         {<<"total_rows">>, length(Matches)}, 
%%                                         {<<"rows">>, Matches}]|Acc], R, S)
%%     end.

get_fields(Doctype, ExFields, R, S) ->
    VQ = #vq{startkey = [Doctype, <<"">>],
             endkey = [Doctype, []]},
    QS = view:to_string(VQ),
    F = fun(X, Acc) ->
                [_, _, Type, _] = jsn:get_value(<<"key">>, X),
                Id = jsn:get_value(<<"id">>, X),
                case(Type =:= <<"fieldset-field">>) and 
                    not (lists:member(Id, ExFields)) of
                    true -> [Id|Acc];
                    _ -> Acc
                end
        end,
    {ok, Json} = couch:get_view_json("fieldsets", "all", QS, R, S),
    lists:foldl(F, [], jsn:get_value(<<"rows">>, Json)).

                
