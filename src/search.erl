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

-export([values/3]).

-include_lib("types.hrl").

%% @doc Perform a search, saving results to ETS table.
do_search(Doctype, RE, Field, TID, Project, S) ->
    {ok, Json} = q:search(Doctype, Field, Project, S),
    Matches = filter(jsn:get_value(<<"rows">>, Json), RE, []),
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

%% @doc simple filter
filter([], _RE, Acc) ->
    Acc;
filter([H|T], RE, Acc) ->
    [_, _, _, K] = jsn:get_value(<<"key">>, H),
    case re:run(K, RE) of
        nomatch -> filter(T, RE, Acc);
        _ -> filter(T, RE, [jsn:set_value(<<"key">>, K, H)|Acc])
    end.

%% @doc Get the fields to search when search all fields or excluding
%% fields.
get_fields(Doctype, ExFields, Project, S) ->
    F = fun(X, Acc) ->
                [_, _, Type, _] = jsn:get_value(<<"key">>, X),
                Id = jsn:get_value(<<"id">>, X),
                case(Type =:= <<"fieldset-field">>) and 
                    not (lists:member(Id, ExFields)) of
                    true -> [Id|Acc];
                    _ -> Acc
                end
        end,
    {ok, Json} = q:fieldset(Doctype, false, Project, S),
    lists:foldl(F, [], jsn:get_value(<<"rows">>, Json)).

%% @doc filter for searches on indexes
index_filter([], _RE, Acc) ->
    lists:reverse(Acc);
index_filter([Row|T], RE, Acc) ->
    case index_filter2(jsn:get_value(<<"key">>, Row), RE) of
        false -> index_filter(T, RE, Acc);
        _ -> index_filter(T, RE, [Row|Acc])
    end.

index_filter2([], _RE) ->
    false;
index_filter2([[_,K]|T], RE) ->
    case re:run(K, RE) of
        nomatch -> index_filter2(T, RE);
        _ -> true
    end.

%% @doc filter for inverted searches on indexes
index_inverted_filter([], _Query, Acc) ->
    lists:reverse(Acc);
index_inverted_filter([Row|T], Query, Acc) ->
    case index_inverted_filter2(jsn:get_value(<<"key">>, Row), Query) of
        false -> index_inverted_filter(T, Query, Acc);
        _ -> index_inverted_filter(T, Query, [Row|Acc])
    end.

index_inverted_filter2([], _Query) ->
    false;
index_inverted_filter2([[_,K]|T], Query) ->
    {ok, RE} = re:compile(K, [unicode]),
    case re:run(Query, RE) of
        nomatch -> index_inverted_filter2(T, Query);
        _ -> true
    end.

%% @doc inverted filter
inverted_filter([], _Query, Acc) ->
    Acc;
inverted_filter([H|T], Query, Acc) ->
    [_, _, SK, K] = jsn:get_value(<<"key">>, H),
    {ok, RE} = re:compile(K, [unicode]),
    case re:run(Query, RE) of
        nomatch -> inverted_filter(T, Query, Acc);
        _ -> inverted_filter(T, Query, [jsn:set_value(<<"key">>, [[SK, K]], H)|Acc])
    end.

%% @doc Assuming that there is more than one field that was searched,
%% compute the totals.
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

%% @doc Retrieve values for a search.
-spec values(sparams(), string(), h:req_state()) -> jsn:json_term().
% Empty case
values(#sparams{qs=[]}, _Project, _S) ->
    [{<<"rows">>, false}];
% Normal case for searching over and index
values(#sparams{qs=Query, index=Index, invert=false}, Project, S) when is_list(Index) ->
    {ok, RE} = re:compile(list_to_binary(Query), [unicode]),
    {ok, Json} = q:index(Index, [], Project, S),
    Rows = jsn:get_value(<<"rows">>, Json),
    Filtered = index_filter(Rows, RE, []),
    [{<<"index_listing">>, true}, {<<"rows">>, Filtered}, 
     {<<"total_rows">>, length(Filtered)}];
% Inverted search over index
values(#sparams{qs=Query, index=Index, invert=true}, Project, S) when is_list(Index) ->
    {ok, Json} = q:index(Index, [], Project, S),
    Rows = jsn:get_value(<<"rows">>, Json),
    Filtered = index_inverted_filter(Rows, list_to_binary(Query), []),
    [{<<"index_listing">>, true}, {<<"rows">>, Filtered}, {<<"total_rows">>, length(Filtered)}];
% Inverted search over field
values(#sparams{doctype=Doctype, qs=Query, fields=[Field], invert=true}, Project, S) ->
    {ok, Json} = q:search(Doctype, Field, Project, S),
    Rows = jsn:get_value(<<"rows">>, Json),
    Filtered = inverted_filter(Rows, list_to_binary(Query), []),
    [{<<"index_listing">>, true}, {<<"rows">>, Filtered}, {<<"total_rows">>, length(Filtered)}];
% Search all fields
values(P=#sparams{doctype=Doctype, fields=[], exclude=false}, Project, S) ->
    NewFields = get_fields(Doctype, [], Project, S),
    values(P#sparams{fields=NewFields}, Project, S);
% Exclude fields
values(P=#sparams{doctype=Doctype, fields=Fields, exclude=true}, Project, S) ->
    NewFields = get_fields(Doctype, Fields, Project, S),
    values(P#sparams{fields=NewFields, exclude=false}, Project, S);
% Search multiple fields
values(#sparams{qs=Query, doctype=Doctype, fields=Fields}, Project, S) ->
    {ok, RE} = re:compile(list_to_binary(Query), [unicode]),
    TID = ets:new(list_to_atom("search-" ++ utils:uuid()), [public]),
    ets:insert(TID, {total, 0}),
    F = fun(X) ->
                do_search(Doctype, RE, X, TID, Project, S)
        end,
    ok = utils:peach(F, Fields, 10),
    Complete = prep_ret(TID),
    ets:delete(TID),
    Complete.
