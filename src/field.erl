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
%%% @doc This module contains functions for manipulating fields

-module(field).

-export([
         from_json/1,
         from_json/2,
         get/2,
         get/3,
         is_meta/1,
         meta_field/1,
         option_list/2,
         update_merge/2,
         set_sortkeys/3,
         to_json/2,
         touch/3,
         touch_all/4
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").
-include_lib("include/types.hrl").

option_list(R, S) ->
    case wrq:path_info(fieldset, R) of
        "metadata" -> meta_options();
        Fieldset -> 
            {ok, Json} = couch:get_view_json(Fieldset, "fields_simple", R, S),
            Json
    end.

is_meta(Id) when is_list(Id) ->
    is_meta(list_to_binary(Id));
is_meta(Id) ->
    is_meta(Id, jsn:get_value(<<"rows">>, meta_options())).

is_meta(_Id, []) ->
    false;
is_meta(Id, [H|T]) ->
    case jsn:get_value(<<"id">>, H) of
        Id -> true;
        _ -> is_meta(Id, T)
    end.

meta_options() ->
    [{<<"rows">>, [[{<<"key">>, <<"Created by">>}, 
                    {<<"value">>, <<"created_by_">>},
                    {<<"id">>, <<"created_by_">>}],
                   [{<<"key">>, <<"Updated by">>}, 
                    {<<"value">>, <<"updated_by_">>},
                    {<<"id">>, <<"updated_by_">>}]]}].

meta_field(Id) when is_list(Id) ->
    meta_field(list_to_binary(Id));
meta_field(Id) when Id =:= <<"created_by_">> ->
    user_field(Id, <<"Created By">>);
meta_field(Id) when Id =:= <<"updated_by_">> -> 
    user_field(Id, <<"Updated By">>).

user_field(Id, Label) ->
    Field = #field{id = Id,
                   label = Label,
                   allowed = couch:user_list(),
                   subcategory = select},
    to_json(Field).
            
-spec touch_all([docfield()], [binary()], utils:reqdata(), any()) -> [docfield()].
touch_all(Dfs, FIds, R, S) ->
    Dfs2 = add_missing(Dfs, FIds, []),
    touch_all2(Dfs2, [], R, S).

touch_all2([], Acc, _R, _S) ->
    lists:reverse(Acc);
touch_all2([Df|Rest], Acc, R, S) ->
    case touch(Df, R, S) of
        undefined -> touch_all2(Rest, Acc, R, S);
        Val -> touch_all2(Rest, [Val|Acc], R, S)
    end.

%% @doc Given the current set of fields in a document, a list of ids
%% and an empty accumulator, make sure that the current fields are
%% complete according to the latest configuration and in the correct
%% order.
-spec add_missing([docfield()], [binary()], []) -> [docfield()].
add_missing(_Current, [], Complete) ->
    lists:reverse(Complete);
add_missing(Current, [Required|Rest], Acc) ->
    case find_required(Required, Current) of
        undefined ->
            add_missing(Current, Rest, [#docfield{id=Required}|Acc]);
        Found ->
            add_missing(Current, Rest, [Found|Acc])
    end.

%% @doc Search for a field with a particular id in a list of
%% docfields.
-spec find_required(binary(), [docfield()]) -> docfield() | undefined.
find_required(_, []) ->    
    undefined;
find_required(Id, [Found=#docfield{id=Id}|_]) ->
    Found;
find_required(Id, [_|Rest]) ->
    find_required(Id, Rest).

-spec touch(Df :: docfield(), R :: utils:reqdata(), S :: any()) -> docfield().
touch(Df, R, S) ->
    touch2(update(Df, R, S), R, S).

%% @doc Set the sortkeys for fields
-spec set_sortkeys(jsn:json_term(), R :: utils:reqdata(), S :: any()) -> jsn:json_term().
set_sortkeys([], _R, _S) ->
    [];
set_sortkeys(Fields, R, S) ->
    set_sortkeys(Fields, [], R, S).

%% @doc Convert a jsn:json_term() field to a field()
-spec from_json(Json :: jsn:json_term()) -> docfield().
from_json(Json) ->
    Subcategory = get_subcategory(jsn:get_value(<<"subcategory">>, Json)),
    #field{
            id = get_value(<<"_id">>, Json),
            rev = get_value(<<"_rev">>, Json),
            allowed = get_value(<<"allowed">>, Json),
            category = field,
            charseq = get_value(<<"charseq">>, Json),
            default = convert_value(Subcategory, Json, <<"default">>),
            description = proplists:get_value(<<"description">>, Json, null),
            doctype = proplists:get_value(<<"doctype">>, Json, null),
            fieldset = get_value(<<"fieldset">>, Json),
            head = get_value(<<"head">>, Json, false),
            label = proplists:get_value(<<"label">>, Json, null),
            max = convert_value(Subcategory, Json, <<"max">>),
            min = convert_value(Subcategory, Json, <<"min">>),
            name = proplists:get_value(<<"name">>, Json, null),
            order = get_value(<<"order">>, Json),
            regex = proplists:get_value(<<"regex">>, Json, null),
            required = get_value(<<"required">>, Json, false),
            reversal = get_value(<<"reversal">>, Json, false),
            source = proplists:get_value(<<"source">>, Json, null),
            subcategory = Subcategory
          }.

%% @doc Convert a jsn:json_term() field within a document to a
%% docfield()
-spec from_json(doc, Json :: jsn:json_term()) -> docfield().
from_json(doc, Json) ->
    Subcategory = get_subcategory(jsn:get_value(<<"subcategory">>, Json)),
    #docfield{
               id = get_value(<<"id">>, Json),
               instance = get_value(<<"instance">>, Json),
               charseq = get_value(<<"charseq">>, Json),
               name = get_value(<<"name">>, Json),
               label = get_value(<<"label">>, Json),
               order = get_value(<<"order">>, Json),
               head = get_value(<<"head">>, Json, false),
               max = convert_value(Subcategory, Json, <<"max">>),
               min = convert_value(Subcategory, Json, <<"min">>),
               regex = proplists:get_value(<<"regex">>, Json, null),
               required = get_value(<<"required">>, Json, false),
               reversal = get_value(<<"reversal">>, Json, false),
               subcategory = Subcategory,
               value = convert_value(Subcategory, Json),
               sortkey = get_value(<<"sortkey">>, Json)
             }.

%% @doc Convert field() to jsn:term()
to_json(F) ->  
    [{<<"_id">>, F#field.id},
     {<<"_rev">>, F#field.rev},
     {<<"allowed">>, F#field.allowed},
     {<<"category">>, field},
     {<<"charseq">>, maybe_binary(F#field.charseq)},
     {<<"default">>, F#field.default},
     {<<"description">>, F#field.description},
     {<<"doctype">>, F#field.doctype},
     {<<"fieldset">>, <<"metadata">>},
     {<<"head">>, F#field.head},
     {<<"label">>, F#field.label},
     {<<"max">>, unconvert_value(F#field.subcategory, F#field.max)},
     {<<"min">>, unconvert_value(F#field.subcategory, F#field.min)},
     {<<"name">>, F#field.name},
     {<<"order">>, F#field.order},
     {<<"regex">>, F#field.regex},
     {<<"required">>, F#field.required},
     {<<"reversal">>, F#field.reversal},
     {<<"source">>, F#field.source},
     {<<"subcategory">>, atom_to_binary(F#field.subcategory, utf8)}].
    
%% @doc Convert a docfield() record within a document() to a
%% jsn:json_term() fieldset 
-spec to_json(doc, F :: docfield()) -> Json :: jsn:json_term().
to_json(doc, F) ->
    [{<<"id">>, F#docfield.id},
     {<<"name">>, F#docfield.name},
     {<<"label">>, F#docfield.label},
     {<<"head">>, F#docfield.head},
     {<<"reversal">>, F#docfield.reversal},
     {<<"required">>, F#docfield.required},
     {<<"min">>, unconvert_value(F#docfield.subcategory, F#docfield.min)},
     {<<"max">>, unconvert_value(F#docfield.subcategory, F#docfield.max)},
     {<<"instance">>, F#docfield.instance},
     {<<"charseq">>, maybe_binary(F#docfield.charseq)},
     {<<"regex">>, F#docfield.regex},
     {<<"order">>, F#docfield.order},
     {<<"subcategory">>, atom_to_binary(F#docfield.subcategory, utf8)},
     {<<"value">>, unconvert_value(F#docfield.subcategory, F#docfield.value)},
     {<<"sortkey">>, F#docfield.sortkey}].
  
%% @doc Get a field() using a field id and a project id
-spec get(Project :: string(), Id :: string) -> fieldset() | {error, Reason :: term()}.
get(Project, Id) ->
    Url = ?ADMINDB ++ Project ++ "/" ++ Id,
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _, Raw} -> convert_field(jsn:decode(Raw));
        {ok, Status, _, _} -> {error, "HTTP status " ++ Status};
        Error -> Error
    end.

%% @doc Get a field() using a fieldset docfield() and webmachine state
-spec get(Df :: docfield(), R :: utils:reqdata(), S :: any()) -> fieldset().
get(Df, R, S) ->
    case proplists:get_value(table_id, S) of
        undefined -> get_from_couch(Df, R, S);
        Tid -> get_from_table(Tid, Df, R, S)
    end.

get_from_table(Tid, Df, R, S) ->
    case ets:lookup(Tid, Df#docfield.id) of
        [] ->
            Val = get_from_couch(Df, R, S),
            true = ets:insert(Tid, {Df#docfield.id, Val}),
            Val;
        [{_, Val}] -> Val
    end.
  
get_from_couch(Df, R, S) ->
    case couch:get_json(safer, binary_to_list(Df#docfield.id), R, S) of
        undefined -> undefined;
        {error, req_timedout} -> get(Df, R, S);
        Val -> from_json(Val)
    end.

%% @doc Take a field() and a docfield() and return a docfiled() updated so
%% that shared items match and sortkey is updated.
-spec update_merge(DF :: docfield(), F :: field()) -> docfield() | {error, Reason :: term()}.
update_merge(Df, F) ->
    case {F#field.id, Df#docfield.id} of
        {Id, Id} -> do_merge(Df, F);
        _Else -> {error, "Id's do not match."}
    end.

-spec update(Df :: docfield(), R :: utils:reqdata(), S :: any()) -> docfieldset() | undefined.
update(Df, R, S) ->
    case get(Df, R, S) of
        undefined -> undefined;
        Val -> update_merge(Df, Val)
    end.

-spec touch2(Df :: docfield() | undefined, R :: utils:reqdata(), S :: any()) ->  jsn:json_term() | undefined.
touch2(undefined, _R, _S) ->
    undefined;
touch2(Df, R, S) ->
    Df#docfield{sortkey=charseq:get_sortkey(Df, R, S)}.

-spec maybe_binary(B :: term()) -> binary() | null.
maybe_binary(<<>>) -> null;
maybe_binary(B) when is_binary(B) -> B;
maybe_binary(_) -> null.

-spec unconvert_value(subcategory(), Value :: term()) -> jsn:json_term().
unconvert_value(date, today) -> <<"today">>;
unconvert_value(date, {Y, M, D}) ->
    list_to_binary(string:right(integer_to_list(Y), 4, $0) ++ "-" ++ 
                       string:right(integer_to_list(M), 2, $0) ++ "-" ++ 
                       string:right(integer_to_list(D), 2, $0));
unconvert_value(_, Value) -> Value.

-spec convert_value(subcategory(), Json :: jsn:json_term()) -> anyval().
convert_value(S, Json) ->
    convert_value(S, Json, <<"value">>).
  
-spec convert_value(subcategory(), Json :: jsn:json_term(), Key :: binary()) -> anyval().
convert_value(boolean, Json, Key) ->
    get_value(Key, Json, false);
convert_value(openboolean, Json, Key) ->
    case get_value(Key, Json) of
        <<>> -> null;
        Val -> Val
    end;
convert_value(multiselect, Json, Key) ->
    case get_value(Key, Json) of
        <<>> -> [];
        Val -> Val
    end;
convert_value(docmultiselect, Json, Key) ->
    case get_value(Key, Json) of
        <<>> -> [];
        Val -> Val
    end;
convert_value(date, Json, Key) ->
    convert_value2(date, get_value(Key, Json, <<>>));
convert_value(Sub, Json, Key) when Sub /= text, Sub /= textarea ->
    get_value(Key, Json);
convert_value(_, Json, Key) ->
    proplists:get_value(Key, Json, null).
  
-spec convert_value2(subcategory(), Value :: jsn:json_term()) -> anyval().
convert_value2(_, null) -> null;
convert_value2(date, <<>>) -> <<>>;
convert_value2(date, <<"today">>) -> today;
convert_value2(date, Value) when is_binary(Value) ->
    Value1 = binary_to_list(Value),
    case length(Value1) of
        10 -> parse_date(Value1);
        _ -> <<>>
    end;
convert_value2(_, Value) -> Value.

-spec parse_date(string()) -> calendar:date().
parse_date([Y1,Y2,Y3,Y4,_,M1,M2,_,D1,D2]) ->
    {list_to_integer([Y1,Y2,Y3,Y4]), list_to_integer([M1,M2]), 
     list_to_integer([D1,D2])}.

% TODO: this is mostly to guard against bad values already entered
% It should be altered or removed in the future.
-spec get_value(Key :: binary(), Json :: jsn:json_term(), Default :: jsn:json_term()) -> jsn:json_term().
get_value(Key, Json, Default) ->
    case proplists:get_value(Key, Json, Default) of
        <<"null">> -> null;
        <<"undefined">> -> null;
        Value -> Value
    end.
  
-spec get_value(Key :: binary(), Json :: jsn:json_term()) -> jsn:json_term().
get_value(Key, Json) ->
    get_value(Key, Json, null).

-spec do_merge(Df :: docfield(), F :: field()) -> docfield() | {error, Reason :: term()}.
do_merge(Df, F) ->
    Df2 = Df#docfield{
            charseq = F#field.charseq,
            head = F#field.head,
            label = F#field.label,
            max = F#field.max,
            min = F#field.min,
            name = F#field.name,
            order = F#field.order,
            regex = F#field.regex,
            required = F#field.required,
            reversal = F#field.reversal,
            subcategory = F#field.subcategory
           },
    update_normalize(Df, F, Df2).
  
-spec update_normalize(docfield(), F :: field(), DF :: docfield()) -> docfield() | {error, Reason :: term()}.
update_normalize(_, #field{default=X}, DF=#docfield{value=X}) ->
    DF;    
update_normalize(_, #field{default=Def}, DF=#docfield{value=null}) -> 
    DF#docfield{value=Def};
update_normalize(_, #field{default=Def}, DF=#docfield{subcategory=openboolean,value=V}) when
      (V /= true) and (V /= false) -> DF#docfield{value=Def};
update_normalize(_, #field{default=Def}, DF=#docfield{subcategory=S,value=V}) when
      ((S == multiselect) or (S == docmultiselect)) and (not is_list(V)) -> 
    DF#docfield{value=Def};
update_normalize(#docfield{subcategory=S}, _, DF=#docfield{subcategory=S2}) when 
    (S == S2) or
    (((S == text) or
      (S == textarea) or
      (S == select) or
      (S == docselect)) and
     ((S2 == text) or
      (S2 == textarea))) -> DF;
update_normalize(#docfield{subcategory=S}, F, DF=#docfield{subcategory=S2,value=V}) when 
    (((S == integer) or
      (S == rational) or
      (S == boolean) or
      (S == openboolean)) and
     ((S2 == text) or
      (S2 == textarea))) ->
    DF2 = DF#docfield{value=iolist_to_binary(io_lib:format("~p", [V]))},
    update_normalize(#docfield{subcategory=text}, F, DF2);
update_normalize(#docfield{subcategory=date}, F, DF) ->
    update_normalize(#docfield{subcategory=text}, F, 
                     DF#docfield{value=unconvert_value(date, 
                                                       DF#docfield.value)});
update_normalize(#docfield{subcategory=S}, _, DF=#docfield{subcategory=S2}) when
    ((S == docselect) or (S == select)) and 
    ((S2 == docselect) or (S2 == select)) -> DF;
update_normalize(#docfield{subcategory=S}, _, DF=#docfield{subcategory=S2}) when
    ((S == docmultiselect) or (S == multiselect)) and 
    ((S2 == docmultiselect) or (S2 == multiselect)) -> DF;
update_normalize(#docfield{subcategory=rational}, _, DF=#docfield{subcategory=integer,value=V}) ->
  DF#docfield{value=erlang:trunc(V)};
update_normalize(#docfield{subcategory=integer}, _, DF=#docfield{subcategory=rational,value=V}) -> 
    DF#docfield{value=(V / 1)};
update_normalize(#docfield{subcategory=boolean}, _, DF=#docfield{subcategory=openboolean}) -> DF;
update_normalize(#docfield{subcategory=openboolean}, _, DF=#docfield{subcategory=boolean,value=V}) ->
    DF#docfield{value=(V == true)};
update_normalize(_, #field{default=D}, DF) ->
    DF#docfield{value=D}.
  
-spec convert_field(Json :: jsn:json_term()) -> fieldset() | {error, Reason :: term()}.
convert_field(Json) ->  
    case jsn:get_value(<<"category">>, Json) of
        <<"field">> -> from_json(Json);
        Cat -> {error, "Returned document had category " ++ binary_to_list(Cat)}
    end.
       
-spec get_subcategory(binary()) -> subcategory().
get_subcategory(Bin) ->
    case Bin of
        <<"text">> -> text;
        <<"textarea">> -> textarea;
        <<"date">> -> date;
        <<"integer">> -> integer;
        <<"rational">> -> rational;
        <<"boolean">> -> boolean;
        <<"openboolean">> -> openboolean;
        <<"select">> -> select;
        <<"multiselect">> -> multiselect;
        <<"docselect">> -> docselect;
        <<"docmultiselect">> -> docmultiselect;
        <<"file">> -> file
    end.

-spec set_sortkeys([jsn:json_term()] | [docfield()], Acc :: [jsn:json_term()] | [docfield()], R :: utils:reqdata(), S :: any()) -> [jsn:json_term()] | [docfield()].
set_sortkeys([], Acc, _R, _S) ->
    lists:reverse(Acc);
set_sortkeys([Field|Rest], Acc, R, S) when is_list(Field) ->
    set_sortkeys(Rest, [jsn:set_value(<<"sortkey">>, 
                                      charseq:get_sortkey(Field, R, S), 
                                      Field)|Acc], R, S);
set_sortkeys([F=#docfield{}|Rest], Acc, R, S) ->
    set_sortkeys(Rest, [F#docfield{sortkey=charseq:get_sortkey(F, R, S)}|Acc], 
                 R, S).
