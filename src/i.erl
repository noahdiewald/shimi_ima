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
-author('Noah Diewald <noah@diewald.me>').

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

-spec create(h:req_data(), h:req_state()) -> {true, h:req_data(), h:req_state()} | h:req_data().
create(R, S) ->
    Json = proplists:get_value(posted_json, S),
    {Project, R1} = h:project(R),
    case jsn:get_value(<<"category">>, Json) of
        <<"index">> ->
            h:create(Json, R1, S);
        <<"doctype">> ->
            case couch:create(Json, Project, S) of
                {ok, created} ->
                    {ok, _} = create_design(Json, Project, S),
                    {true, R1, S};
                {forbidden, Message} ->
                    {ok, R2} = cowboy_req:reply(403, [], Message, R1),
                    R2
            end
    end.

-spec create_design(jsn:json_term(), string(), h:req_state()) -> couch:ret().
create_design(DocJson, Project, S) ->
    DocId = jsn:get_value(<<"_id">>, DocJson),
    Design = get_design(binary_to_list(DocId), Project, S),
    couch:create(Design, Project, [{admin, true}|S]).

-spec get_design(string(), string(), h:req_state()) -> jsn:json_term().
get_design(Id, Project, S) ->
    {ok, Json} = q:index_design(Id, Project, S),
    [Row|[]] = jsn:get_value(<<"rows">>, Json),
    jsn:decode(jsn:get_value(<<"value">>, Row)).
            
-spec update(h:req_data(), h:req_state()) -> {true, h:req_data(), h:req_state()} | h:req_data().
update(R, S) ->
    {[Id, Rev, Project], R1} = h:g([id, rev, project], R),
    {ok, Body, R2} = cowboy_req:body(R1),
    Json = jsn:decode(Body),
    Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
    Json2 = jsn:set_value(<<"_rev">>, list_to_binary(Rev), Json1),
    
    Json3 = case jsn:get_value(<<"conditions">>, Json) of
        undefined ->
            Json2;
        Conditions ->
            Expression = conditions:trans(Conditions),
            jsn:set_value(<<"expression">>, Expression, Json2)
    end,
    
    case couch:update(Id, Json3, Project, S) of
        {ok, updated} -> 
            {ok, _} = update_design(Id, Project, S),
            {true, R2, S};
        {forbidden, Message} ->
            {ok, R3} = cowboy_req:reply(403, [], Message, R2),
            R3;
        {error, conflict} ->
            Msg = <<"This document has been updated or deleted by another user.">>,
            Msg1 = jsn:encode([{<<"message">>, Msg}]),
            {ok, R3} = cowboy_req:reply(409, [], Msg1, R2),
            R3
    end.

-spec update_design(string(), string(), h:req_state()) -> {true, h:req_data(), h:req_state()} | h:req_data().
update_design(DocId, Project, S) ->
    DesignId = "_design/" ++ DocId,
    Design = get_design(DocId, Project, S),
    case h:get(DesignId, Project, S) of
        {error, not_found} ->
            create_design(Design, Project, [{admin, true}|S]);
        {ok, PrevDesign} ->
            Rev = jsn:get_value(<<"_rev">>, PrevDesign),
            Design2 = jsn:set_value(<<"_rev">>, Rev, Design),
            couch:update(DesignId, Design2, Project, [{admin, true}|S])
    end.
    
-spec view(h:req_data(), h:req_state()) -> {iolist(), h:req_data(), h:req_state()} | h:req_data().
view(R, S) ->
    Msg = <<"still building. Please wait 5 to 10 minutes and try again.">>,
    Item = <<"Index">>,
    Message = jsn:encode([{<<"message">>, Msg}, {<<"fieldname">>, Item}]),
    {Limit, R1} = cowboy_req:qs_value(<<"limit">>, R),
    case get_index(R1, S) of
        {{ok, Json}, Info, R2} ->
            Index = add_encoded_keys(Json),
            Vals = [{<<"limit">>, Limit}|Index] ++ Info,
            {ok, Html} = render:render(document_index_dtl, Vals),
            {Html, R2, S};
        {{error, not_found}, _, R2} ->
            {<<"">>, R2, S};
        {{error, req_timedout}, _, R2} ->
            {ok, R3} = cowboy_req:reply(504, [], Message, R2),
            R3
    end.

-spec get_index(h:req_data(), h:req_state()) -> {couch:ret(), jsn:json_term(), h:req_data()}.
get_index(R, S) ->
    {[Id, Index], R1} = h:g([id, index], R),
    case {Id, Index} of
        {undefined, undefined} ->
            {Doctype, R2} = h:doctype(R1),
            {Ret, R3} = q:index(Doctype, R2, S),
            {Info, R4} = h:basic_info("", " Index", R3, S),
            {Ret, Info, R4};
        {IndexId, undefined} -> 
            {Ret, R2} = q:index(IndexId, R1, S),
            {Ret, [], R2};
        {undefined, IndexId} ->
            {Ret, R2} = q:index(IndexId, R1, S),
            {Info, R3} = h:basic_info("", " Index", R2, S),
            {Ret, Info, R3}
    end.
