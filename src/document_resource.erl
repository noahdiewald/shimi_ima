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
%%% @doc Dictionary Maker resource for dealing with documents.

-module(document_resource).

% Webmachine API
-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         create_path/2,
         delete_resource/2,
         from_json/2,
         init/1, 
         is_authorized/2,
         post_is_create/2,
         resource_exists/2,
         to_html/2,
         to_json/2
        ]).

% Custom
-export([
         validate_authentication/3
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("types.hrl").

% Standard webmachine functions

init(Opts) -> {ok, Opts}.

resource_exists(R, S) ->
    case proplists:get_value(target, S) of
        identifier -> {h:exists(h:id(R), R, S), R, S};
        revision -> {h:exists(h:id(R), R, S), R, S};
        _ -> {h:exists(h:doctype(R), R, S), R, S}
    end. 

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    case proplists:get_value(target, S) of
        main -> {['HEAD', 'GET', 'POST'], R, S};
        index -> {['HEAD', 'GET'], R, S};
        identifier -> {['HEAD', 'GET', 'PUT', 'DELETE'], R, S};
        revision -> {['HEAD', 'GET'], R, S};
        edit -> {['HEAD', 'GET'], R, S};
        search -> {['HEAD', 'GET'], R, S};
        ws_get -> {['HEAD', 'GET'], R, S};
        ws_put -> {['HEAD', 'PUT'], R, S}
    end.
  
delete_resource(R, S) ->
    {ok, Json} = h:rev_data(R, S),
    case jsn:get_value(<<"deleted_">>, Json) of
        true ->
            json_update(jsn:set_value(<<"deleted_">>, false, Json), R, S);
        _ ->
            json_update(jsn:set_value(<<"deleted_">>, true, Json), R, S)
    end.
  
post_is_create(R, S) ->
    {true, R, S}.

create_path(R, S) ->
    Json = jsn:decode(wrq:req_body(R)),
  
    Id = utils:uuid(),
    Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
  
    Location = "http://" ++ wrq:get_req_header("host", R) ++ "/" ++ 
        wrq:path(R) ++ "/" ++ Id,
    R1 = wrq:set_resp_header("Location", Location, R),
  
    {Id, R1, [{posted_json, Json1}|S]}.

content_types_provided(R, S) ->
    case proplists:get_value(target, S) of
        ws_get -> {[{"application/json", to_json}], R, S};
        _ -> {[{"text/html", to_html}], R, S}
    end.
  
content_types_accepted(R, S) ->
    {[{"application/json", from_json}], R, S}.
  
to_html(R, S) ->
    case proplists:get_value(target, S) of
        edit -> {html_edit(R, S), R, S};
        main -> {html_documents(R, S), R, S};
        index -> html_index(R, S);
        identifier -> {html_document(R, S), R, S};
        revision -> {html_revision(R, S), R, S};
        search -> {html_search(R, S), R, S}
    end.
    
to_json(R, S) ->
    {json_ws(R, S), R, S}.

from_json(R, S) ->
    case proplists:get_value(target, S) of
        main -> json_create(R, S);
        identifier -> json_update(R, S)
    end.
  
% Helpers
  
json_create(R, S) ->
    Json = proplists:get_value(posted_json, S),
    Json1 = document:set_sortkeys(Json, R, S),
    % Normalization assumes a complete record, which would normally
    % contain a revision
    NormJson = jsn:delete_value(<<"_rev">>, document:normalize(doc, Json1)),
    case couch:create(NormJson, h:project(R), S) of
        {ok, created} -> 
            bump_deps(R, S),
            {true, R, S};
        {forbidden, Message} ->
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 403}, R1, S}
    end.
  
json_update(R, S) ->
    Json = jsn:decode(wrq:req_body(R)),
    Json1 = document:set_sortkeys(Json, R, S),
    json_update(Json1, R, S).
  
json_update(Json, R, S) ->
    Json1 = jsn:set_value(<<"_id">>, list_to_binary(h:id(R)), Json),
    Json2 = jsn:set_value(<<"_rev">>, list_to_binary(h:rev(R)), Json1),
    NormJson = document:normalize(doc, Json2),
  
    case couch:update(h:id(R), NormJson, h:project(R), S) of
        {ok, updated} ->
            {ok, NewJson} = h:id_data(R, S),
            Message = jsn:encode([{<<"rev">>, jsn:get_value(<<"_rev">>, NewJson)}]),
            R1 = wrq:set_resp_body(Message, R),
            bump_deps(R, S),
            {true, R1, S};
        {forbidden, Message} ->
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 403}, R1, S};
        {error, conflict} ->
            Msg = <<"This document has been edited or deleted by another user.">>,
            Message = jsn:encode([{<<"message">>, Msg}]),
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 409}, R1, S}
    end.

json_ws(R, S) ->
    Docs = jsn:decode(wrq:get_qs_value("set", R)),
    Json = worksheet:get(Docs, h:project(R), S),
    jsn:encode(Json).

html_documents(R, S) ->
    {ok, Html} = render:render(document_dtl, h:basic_info("", " Documents", R, S)),
    Html.

html_edit(R, S) ->
    {ok, Json} = q:fieldset(h:doctype(R), h:project(R), S),
    Fieldsets = fieldset:arrange(jsn:get_value(<<"rows">>, Json), nofields),
    Vals = [{<<"fieldsets">>, Fieldsets}|h:basic_info("Edit or Create ", "", R, S)],
    {ok, Html} = render:render(document_edit_dtl, Vals),
    Html.

html_index(R, S) ->
    i:view(R, S).

html_search(R, S) ->
    Query = wrq:get_qs_value("q", R),
    Fields = case h:field(R) of
        undefined ->
            [];
        Value ->
            jsn:decode(Value)
    end,
    Params = #sparams{
        doctype = h:doctype(R),
        index = h:index(R), 
        fields = Fields,
        exclude = wrq:get_qs_value("exclude", R) =:= "true",
        invert = wrq:get_qs_value("invert", R) =:= "true",
        qs = Query
    },
    Results = search:values(Params, h:project(R), S),
    {ok, Html} = render:render(document_search_dtl, Results),
    Html.

html_document(R, S) ->
    {ok, OrigJson} = h:id_data(R, [{revs_info, true}|S]),
    RevsInfo = proplists:get_value(<<"_revs_info">>, OrigJson),
    NormJson = document:normalize(doc, OrigJson),
    Vals = [{<<"revs_info">>, RevsInfo}|NormJson] ++ h:basic_info("", "", R, S),
    {ok, Html} = render:render(document_view_dtl, Vals),
    Html.

html_revision(R, S) ->
    {ok, Data} = h:rev_data(R, S),
    Requested = document:normalize(doc, Data),
    Prev = case h:id_data(R, S) of
               {ok, Curr} ->
                   CurrRev = jsn:get_value(<<"_rev">>, Curr),
                   ReqRev = jsn:get_value(<<"_rev">>, Requested),
                   CurrRev /= ReqRev;
               _ ->
                   false
           end,
    Json = [{<<"previous_revision">>, Prev}|Requested],
    {ok, Html} = render:render(document_view_tree_dtl, Json),
    Html.

validate_authentication(Props, R, S) ->
    {ok, ProjectData} = h:project_data(R, S),
    Name = jsn:get_value(<<"name">>, ProjectData),
    ValidRoles = [<<"_admin">>, <<"manager">>, Name],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R, S};
        false -> {proplists:get_value(auth_head, S), R, S}
    end.

bump_deps(R, S) ->
    Project = wrq:path_info(project, R),
    Doctype = wrq:path_info(doctype, R),
    spawn(dependent, bump, [Doctype, Project, S]),
    ok.
