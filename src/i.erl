%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of Ʃimi Ima.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% dictionary_maker is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General
%%% Public License along with dictionary_maker. If not, see
%%% <http://www.gnu.org/licenses/>.

%%% @copyright 2012 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>

%%% @doc For manipulating indexes.

-module(i).

-export([
    create/2,
    update/2,
    view/2
    ]).

add_encoded_key(Row) ->
    Key = jsn:get_value(<<"key">>, Row),
    jsn:set_value(<<"encoded_key">>, jsn:to_base64(Key), Row).

%% @doc Add escaped keys to view output
-spec add_encoded_keys(jsn:json_term()) -> jsn:json_term().
add_encoded_keys(Json) ->
    Rows = lists:map(fun add_encoded_key/1, jsn:get_value(<<"rows">>, Json)),
    jsn:set_value(<<"rows">>, Rows, Json).

create(R, S) ->
    Json = proplists:get_value(posted_json, S),
    case jsn:get_value(<<"category">>, Json) of
        <<"index">> ->
            h:create(Json, R, S);
        <<"doctype">> ->
            case couch:create(Json, h:project(R), S) of
                {ok, created} ->
                    {ok, _} = create_design(Json, h:project(R), S),
                    {true, R, S};
                {forbidden, Message} ->
                    R1 = wrq:set_resp_body(Message, R),
                    {{halt, 403}, R1, S}
            end
    end.

create_design(DocJson, Project, S) ->
    DocId = jsn:get_value(<<"_id">>, DocJson),
    Design = get_design(binary_to_list(DocId), Project, S),
    couch:create(Design, Project, [{admin, true}|S]).

get_design(Id, Project, S) ->
    {ok, Json} = q:index_design(Id, Project, S),
    [Row|[]] = jsn:get_value(<<"rows">>, Json),
    jsn:decode(jsn:get_value(<<"value">>, Row)).
            
update(R, S) ->
    Json = jsn:decode(wrq:req_body(R)),
    Json1 = jsn:set_value(<<"_id">>, list_to_binary(h:id(R)), Json),
    Json2 = jsn:set_value(<<"_rev">>, list_to_binary(h:rev(R)), Json1),
    
    Json3 = case jsn:get_value(<<"conditions">>, Json) of
        undefined ->
            Json2;
        Conditions ->
            Expression = conditions:trans(Conditions),
            jsn:set_value(<<"expression">>, Expression, Json2)
    end,
    
    case couch:update(h:id(R), Json3, h:project(R), S) of
        {ok, updated} -> 
            {ok, _} = update_design(R, S),
            {true, R, S};
        {forbidden, Message} ->
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 403}, R1, S};
        {error, conflict} ->
            Msg = <<"This resource has been edited or deleted by another user.">>,
            Message = jsn:encode([{<<"message">>, Msg}]),
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 409}, R1, S}
    end.

update_design(R, S) ->
    Project = h:project(R),
    DocId = h:id(R),
    DesignId = "_design/" ++ DocId,
    Design = get_design(DocId, Project, S),
  
    case h:get(DesignId, R, S) of
        {error, not_found} ->
            couch:create(Design, Project, [{admin, true}|S]);
        {ok, PrevDesign} ->
            Rev = jsn:get_value(<<"_rev">>, PrevDesign),
            Design2 = jsn:set_value(<<"_rev">>, Rev, Design),
            couch:update(DesignId, Design2, Project, [{admin, true}|S])
    end.
    
view(R, S) ->
    Msg = <<"still building. Please wait and try again.">>,
    Item = <<"Index">>,
    Message = jsn:encode([{<<"message">>, Msg}, {<<"fieldname">>, Item}]),
    Limit = wrq:get_qs_value("limit", R),
    case get_index(R, S) of
        {{ok, Json}, Info} ->
            Index = add_encoded_keys(Json),
            Vals = [{<<"limit">>, Limit}|Index] ++ Info,
            {ok, Html} = render:render(document_index_dtl, Vals),
            {Html, R, S};
        {{error, not_found}, _} ->
            {<<"">>, R, S};
        {{error, req_timedout}, _} ->
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 504}, R1, S}
    end.

get_index(R, S) ->
    case {h:id(R), h:index(R)} of
        {undefined, undefined} -> 
            {q:index(h:doctype(R), R, S), h:basic_info("", " Index", R, S)};
        {Id, undefined} -> 
            {q:index(Id, R, S), []};
        {undefined, Id} ->
            {q:index(Id, R, S), h:basic_info("", " Index", R, S)}
    end.
