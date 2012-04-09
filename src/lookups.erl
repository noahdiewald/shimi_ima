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
%%% @doc This module contains functions for manipulating documents

-module(lookups).

-export([build/1]).

-include_lib("types.hrl").

-type i_entry() :: [{fieldid(), sortkey_val()  | [sortkey_val()]}].
-type w_i_entry() :: {wrap, i_entry()} | i_entry().
-type index_info() :: {i_entry(), [fieldid()], [fieldid()]}.
-type index_info_wrap() :: {w_i_entry(), [fieldid()], [fieldid()]}.
-type json_fields_() :: [{binary(), jsn:json_term()}].
-type json_fields() :: json_fields_() | [json_fields_()].
-type record_fields() :: [docfield()]  | [[docfield()]].

%% @doc Build and index and other quick lookup information that can be
%% used to optimize search
-spec build([docfieldset()] | jsn:json_term()) -> index_info().
build(Fieldsets=[#docfieldset{}|_]) ->
    for_record(Fieldsets, {[], [], []});
build(Fieldsets) ->
    for_json(Fieldsets, {[], [], []}).

-spec for_record([docfieldset()], index_info()) ->  index_info().
for_record([], {I, H, R}) ->
    {lists:reverse(I), lists:reverse(H), lists:reverse(R)};
for_record([F|T], {I, H, R}) ->
    for_record(T, for_record2(F#docfieldset.fields, {I, H, R})).

-spec for_json(jsn:json_term(), index_info()) -> index_info().
for_json([], {I, H, R}) ->
    {lists:reverse(I), lists:reverse(H), lists:reverse(R)};
for_json([F|T], {I, H, R}) ->
    Fs = case jsn:get_value(<<"multiple">>, F) of
             true ->
                 jsn:get_value(<<"multifields">>, F);
             false ->
                 [{<<"fields">>, jsn:get_value(<<"fields">>, F)}]
         end,
    for_json(T, for_json2(Fs, {I, H, R})).

-spec for_record2(record_fields(), index_info_wrap()) ->  index_info().
for_record2([], {{wrap, I}, H, R}) -> {I, H, R};
for_record2([], Lookups) -> Lookups;
for_record2(Fields=[[_|_]|_], Lookups) -> for_record3(Fields, Lookups);
for_record2([F=#docfield{id=Id,head=true}|T], {I, H, R}) ->
    for_record2([F#docfield{head=false}|T], {I, [Id|H], R});
for_record2([F=#docfield{id=Id,reversal=true}|T], {I, H, R}) ->
    for_record2([F#docfield{reversal=false}|T], {I, H, [Id|R]});
for_record2([#docfield{id=Id,value=V,sortkey=SK}|T], {{wrap, I}, H, R}) ->
    for_record2(T, {{wrap, [{Id, [[SK, V]]}|I]}, H, R});
for_record2([#docfield{id=Id,value=V,sortkey=SK}|T], {I, H, R}) ->
    for_record2(T, {[{Id, [SK, V]}|I], H, R}).

-spec for_json2(json_fields(), index_info_wrap()) ->  index_info().
for_json2([], {{wrap, I}, H, R}) -> {I, H, R};
for_json2([], Lookups) -> Lookups;
for_json2([{<<"fields">>, []}], {{wrap, I}, H, R}) -> {I, H, R};
for_json2([{<<"fields">>, []}], Lookups) -> Lookups;
for_json2(F=[[{<<"fields">>,_}|_]|_], Lookups) ->
    for_json3(F, Lookups);
for_json2([{<<"fields">>, [[{_,Id},N,L,{<<"head">>,true}|Rest]|T]}], 
          {I, H, R}) ->
    for_json2([{<<"fields">>, [[{<<"id">>,Id},N,L,{<<"head">>,false}|Rest]|T]}],
              {I, [Id|H], R});
for_json2([{<<"fields">>, [[{_,Id},N,L,Hd,
                            {<<"reversal">>,true}|Rest]|T]}],{I, H, R}) ->
    for_json2([{<<"fields">>, [[{<<"id">>,Id},N,L,Hd,
                                {<<"reversal">>,false}|Rest]|T]}],
              {I, H, [Id|R]});
for_json2([{<<"fields">>, [[{_,Id},_,_,_,_,_,_,_,_,_,_,_,_,{_,V},{_,SK}]|T]}],
          {{wrap, I}, H, R}) ->
    for_json2([{<<"fields">>, T}], {{wrap, [{Id, [[SK, V]]}|I]}, H, R});
for_json2([{<<"fields">>, [[{_,Id},_,_,_,_,_,_,_,_,_,_,_,_,{_,V},{_,SK}]|T]}],
          {I, H, R}) ->
    for_json2([{<<"fields">>, T}], {[{Id, [SK, V]}|I], H, R}).

-spec for_record3(record_fields(), index_info()) ->  index_info().
for_record3([Fs|T], Lookups) ->
    for_record3(T, Lookups, for_record2(lists:reverse(Fs), {[],[],[]})).

-spec for_record3(record_fields(), index_info(), index_info()) ->  index_info().
for_record3([], {I, H, R}, {I1, H1, R1}) ->
    {I1 ++ I, H1 ++ H, R1 ++ R};
for_record3([Fs|T], Lookups, {I, H, R}) ->
    {I1, _, _} = for_record2(lists:reverse(Fs), {[],[],[]}),
    for_record3(T, Lookups, {merge_lookups(I, I1, []), H, R}).

-spec for_json3(json_fields(), index_info()) ->  index_info().
for_json3([[{<<"fields">>, Fs}]|T], Lookups) ->
    for_json3(T, Lookups, for_json2([{<<"fields">>, lists:reverse(Fs)}],
                                    {[], [], []})).

-spec for_json3(json_fields(), index_info(), index_info()) ->  index_info().
for_json3([], {I, H, R}, {I1, H1, R1}) ->
    {I1 ++ I, H1 ++ H, R1 ++ R};
for_json3([[{<<"fields">>, Fs}]|T], Lookups, {I, H, R}) ->
    {I1, _, _} = for_json2([{<<"fields">>, lists:reverse(Fs)}], {[],[],[]}),
    for_json3(T, Lookups, {merge_lookups(I, I1, []), H, R}).

-spec merge_lookups(i_entry(), i_entry(), []) -> i_entry().
merge_lookups([], [], IAcc) ->
    IAcc;
merge_lookups([{Id, V}|T], [{Id, V2}|T1], IAcc) ->
    merge_lookups(T, T1, [{Id, [V|[V2]]}|IAcc]).
