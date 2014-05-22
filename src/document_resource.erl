%%% Copyright 2012 University of Wisconsin Madison Board of Regents.
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
%%% @doc Dictionary Maker resource for dealing with documents.

-module(document_resource).
-author('Noah Diewald <noah@diewald.me>').

-export([init/3]).
-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2,
         from_json/2,
         is_authorized/2,
         resource_exists/2,
         rest_init/2,
         to_html/2,
         to_json/2
        ]).
-export([
         validate_authentication/3
        ]).

-include_lib("types.hrl").

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

resource_exists(R, S) ->
    {[Doctype, Project], R1} = h:g([doctype, project], R),
    S1 = [{doctype, Doctype}, {project, Project}|S],
    case proplists:get_value(target, S) of
        identifier -> h:exists_id(R1, S1);
        revision -> h:exists_id(R1, S1);
        _ -> h:exists_with_deps([Doctype], R1, S)
    end. 

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    case proplists:get_value(target, S) of
        main -> {[<<"HEAD">>, <<"GET">>, <<"POST">>], R, S};
        index -> {[<<"HEAD">>, <<"GET">>], R, S};
        identifier -> {[<<"HEAD">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], R, S};
        revision -> {[<<"HEAD">>, <<"GET">>], R, S};
        search -> {[<<"HEAD">>, <<"GET">>], R, S};
        worksheets_get -> {[<<"HEAD">>, <<"POST">>], R, S};
        worksheets_put -> {[<<"HEAD">>, <<"GET">>], R, S}
    end.
  
content_types_accepted(R, S) ->
    h:accept_json(R, S).

content_types_provided(R, S) ->
    case proplists:get_value(target, S) of
        worksheets_get -> {[{{<<"application">>, <<"json">>, []}, to_json}], R, S};
        identifier -> {[{{<<"application">>, <<"json">>, []}, to_json}], R, S};
        revision -> {[{{<<"application">>, <<"json">>, []}, to_json}], R, S};
        index -> {[{{<<"application">>, <<"json">>, []}, to_json}], R, S};
        search -> {[{{<<"application">>, <<"json">>, []}, to_json}], R, S};
        _ -> {[{{<<"text">>, <<"html">>, []}, to_html},
                   {{<<"application">>, <<"json">>, []}, to_json}], R, S}
    end.
  
delete_resource(R, S) ->
    {{ok, Json}, R1} = h:rev_data(R, S),
    case jsn:get_value(<<"deleted_">>, Json) of
        true ->
            S1 = [{change_type, restored}|S],
            json_update(jsn:set_value(<<"deleted_">>, false, Json), R1, S1);
        _ ->
            S1 = [{change_type, deleted}|S],
            json_update(jsn:set_value(<<"deleted_">>, true, Json), R1, S1)
    end.
  
to_html(R, S) ->
    case proplists:get_value(target, S) of
        main -> html_documents(R, S)
    end.
    
to_json(R, S) ->
    case proplists:get_value(target, S) of
        main -> json_index(R, S);
        index -> json_index(R, S);
        search -> json_search(R, S);
        identifier -> json_document(R, S);
        revision -> json_revision(R, S)
    end.

from_json(R, S) ->
    case proplists:get_value(target, S) of
        worksheets_get ->
            json_ws(R, S);
        main -> 
            S1 = [{change_type, created}|S],
            json_create(R, S1);
        identifier -> 
            S1 = [{change_type, updated}|S],
            json_update(R, S1)
    end.
  
% Helpers
  
json_create(R, S) ->
    {[Project, Doctype], R1} = h:g([project, doctype], R),
    {R2, S1} = h:extract_create_data(R1, S),
    Json = proplists:get_value(posted_json, S1),
    Json1 = document:set_sortkeys(Json, Project, S1),
    % Normalization assumes a complete record, which would normally
    % contain a revision
    NormJson = jsn:delete_value(<<"_rev">>, document:normalize(doc, Json1)),
    case couch:create(NormJson, Project, S1) of
        {ok, Data} ->
            spawn(change_log, proplists:get_value(change_type, S1), [Data, Doctype, Project, S1]),
            R3 = bump_deps(R2, S1),
            {{true, proplists:get_value(create_path, S1)}, R3, S1};
        {forbidden, Message} ->
            {ok, R3} = cowboy_req:reply(403, [], Message, R2),
            {halt, R3, S1}
    end.

json_index(R, S) ->  
    i:view(R, S).

json_update(R, S) ->
    {Project, R1} = h:project(R),
    {ok, Body, R2} = cowboy_req:body(R1),
    Json = jsn:decode(Body),
    Json1 = document:set_sortkeys(Json, Project, S),
    json_update(Json1, R2, S).
  
json_update(Json, R, S) ->
    {[Id, Rev, Project, Doctype], R1} = h:g([id, rev, project, doctype], R),
    Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
    Json2 = jsn:set_value(<<"_rev">>, list_to_binary(Rev), Json1),
    NormJson = document:normalize(doc, Json2),
  
    case couch:update(Id, NormJson, Project, S) of
        {ok, Data} ->
            NewRev = jsn:get_value(<<"document_revision">>, Data),
            Message = jsn:encode([{<<"rev">>, NewRev}]),
            R2 = cowboy_req:set_resp_body(Message, R1),
            spawn(change_log, proplists:get_value(change_type, S), [Data, Doctype, Project, S]),
            R3 = bump_deps(R2, S),
            {true, R3, S};
        {forbidden, Message} ->
            {ok, R2} = cowboy_req:reply(403, [], Message, R1),
            {halt, R2, S};
        {error, conflict} ->
            Msg = <<"This document has been updated or deleted by another user.">>,
            Msg1 = jsn:encode([{<<"message">>, Msg}]),
            {ok, R2} = cowboy_req:reply(409, [], Msg1, R1),
            {halt, R2, S}
    end.

json_ws(R, S) ->
    {ok, Body, R1} = cowboy_req:body(R),
    {Project, R2} = h:project(R1),
    Keys = jsn:decode(Body),
    Json = worksheet:get(jsn:encode([{<<"keys">>, Keys}]), Project, S),
    R3 = cowboy_req:set_resp_body(jsn:encode(Json), R2),
    {true, R3, S}.

json_document(R, S) ->
    {{ok, OrigJson}, R1} = h:id_data(R, [{revs_info, true}|S]),
    {Info, R2} = h:basic_info(R1, S),
    [First|RevsInfo] = proplists:get_value(<<"_revs_info">>, OrigJson),
    F = fun (This=[{<<"rev">>, Rev}, {<<"status">>, Status}]) ->
        {_, [$-|Count]} = lists:split(32, lists:reverse(binary_to_list(Rev))),
        [{<<"rev">>, Rev}, {<<"status">>, Status =:= <<"available">>}, {<<"count">>, list_to_binary(lists:reverse(Count))}, {<<"first">>, This =:= First}]
    end,
    RevsInfo2 = lists:map(F, [First|RevsInfo]),
    NormJson = document:normalize(doc, OrigJson),
    Vals = [{<<"revs_info">>, RevsInfo2}|NormJson] ++ Info,
    {jsn:encode(Vals), R2, S}.

json_revision(R, S) ->
    {{ok, RevData}, R1} = h:rev_data(R, S),
    {IdData, R2} = h:id_data(R1, S),
    Requested = document:normalize(doc, RevData),
    Prev = case IdData of
               {ok, Curr} ->
                   CurrRev = jsn:get_value(<<"_rev">>, Curr),
                   ReqRev = jsn:get_value(<<"_rev">>, Requested),
                   CurrRev /= ReqRev;
               _ ->
                   false
           end,
    Json = [{<<"previous_revision">>, Prev}|Requested],
    {jsn:encode(Json), R2, S}.

json_search(R, S) ->
    {Query, R1} = cowboy_req:qs_val(<<"q">>, R),
    {Exclude, R2} = cowboy_req:qs_val(<<"exclude">>, R1),
    {Invert, R3} = cowboy_req:qs_val(<<"invert">>, R2),
    {[Project, Doctype, Index, Field], R4} = h:g([project, doctype, index, field], R3),
    Fields = case Field of
        undefined ->
            [];
        Value ->
            jsn:decode(Value)
    end,
    Params = #sparams{
        doctype = Doctype,
        index = Index,
        fields = Fields,
        exclude = Exclude =:= <<"true">>,
        invert = Invert =:= <<"true">>,
        qs = Query
    },
    Results = search:values(Params, Project, S),
    {jsn:encode(Results), R4, S}.

html_documents(R, S) ->
    {Info, R1} = h:basic_info(R, S),
    {ok, Html} = render:render(document_dtl, Info),
    {Html, R1, S}.

validate_authentication(Props, R, S) ->
    {{ok, ProjectData}, R1} = h:project_data(R, S),
    Name = jsn:get_value(<<"name">>, ProjectData),
    ValidRoles = [<<"_admin">>, <<"manager">>, Name],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R1, S};
        false -> {proplists:get_value(auth_head, S), R1, S}
    end.

bump_deps(R, S) ->
    {[Project, Doctype], R1} = h:g([project, doctype], R),
    spawn(dependent, bump, [Doctype, Project, S]),
    R1.
