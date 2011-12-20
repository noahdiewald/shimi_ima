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
  to_html/2
]).

% Custom
-export([
  validate_authentication/3
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

% Standard webmachine functions

init(Opts) -> {ok, Opts}.

resource_exists(R, S) ->
  Doctype = wrq:path_info(doctype, R),
  Id = wrq:path_info(id, R),
  
  case proplists:get_value(target, S) of
    identifier -> {couch:exists(Id, R, S), R, S};
    _ -> {couch:exists(Doctype, R, S), R, S}
  end. 

is_authorized(R, S) ->
  proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
  case proplists:get_value(target, S) of
    main -> {['HEAD', 'GET', 'POST'], R, S};
    index -> {['HEAD', 'GET'], R, S};
    identifier -> {['HEAD', 'GET', 'PUT', 'DELETE'], R, S};
    edit -> {['HEAD', 'GET'], R, S}
  end.
  
delete_resource(R, S) ->
  Json = couch:get_json(rev, R, S),
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
  
  {ok, Id} = couch:get_uuid(R, S),
  Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
  
  Location = "http://" ++ wrq:get_req_header("host", R) ++ "/" ++ wrq:path(R) ++ "/" ++ Id,
  R1 = wrq:set_resp_header("Location", Location, R),
  
  {Id, R1, [{posted_json, Json1}|S]}.

content_types_provided(R, S) ->
  {[{"text/html", to_html}], R, S}.
  
content_types_accepted(R, S) ->
  {[{"application/json", from_json}], R, S}.
  
to_html(R, S) ->
  case proplists:get_value(target, S) of
    edit -> {html_edit(R, S), R, S};
    main -> {html_documents(R, S), R, S};
    index -> {html_index(R, S), R, S};
    identifier -> {html_document(R, S), R, S}
  end.
  
from_json(R, S) ->
  case proplists:get_value(target, S) of
    main -> json_create(R, S);
    identifier -> json_update(R, S)
  end.
  
% Helpers
  
json_create(R, S) ->
  Json = proplists:get_value(posted_json, S),
  Json1 = document:set_sortkeys(Json, R, S),
  case couch:create(doc, jsn:encode(Json1), R, S) of
    {ok, created} -> {true, R, S};
    {403, Message} ->
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 403}, R1, S}
  end.
  
json_update(R, S) ->
  Json = jsn:decode(wrq:req_body(R)),
  Json1 = document:set_sortkeys(Json, R, S),
  json_update(Json1, R, S).
  
json_update(Json, R, S) ->
  Id = wrq:path_info(id, R),
  Rev = wrq:get_qs_value("rev", R),
  Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
  Json2 = jsn:set_value(<<"_rev">>, list_to_binary(Rev), Json1),
  
  case couch:update(doc, Id, jsn:encode(Json2), R, S) of
    {ok, updated} ->
      NewJson = couch:get_json(Id, R, S),
      Message = jsn:encode([{<<"rev">>, jsn:get_value(<<"_rev">>, NewJson)}]),
      R1 = wrq:set_resp_body(Message, R),
      {true, R1, S};
    {403, Message} ->
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 403}, R1, S};
    {409, _} ->
      Message = jsn:encode([{<<"message">>, <<"This document has been edited or deleted by another user.">>}]),
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 409}, R1, S}
  end.

html_documents(R, S) ->
  Doctype = wrq:path_info(doctype, R),
  
  Vals = [
    {<<"title">>, list_to_binary(Doctype ++ " Documents")}, 
    {<<"project_info">>, couch:get_json(project, R, S)},
    {<<"doctype_info">>, couch:get_json(doctype, R, S)},
    {<<"user">>, proplists:get_value(user, S)}
  ],
  
  {ok, Html} = document_dtl:render(Vals),
  Html.

html_edit(R, S) ->
  Doctype = wrq:path_info(doctype, R),
  {ok, Json} = couch:get_view_json(Doctype, "fieldsets", R, S),
  
  Vals = [
    {<<"title">>, list_to_binary("Edit or Create " ++ Doctype)}, 
    {<<"project_info">>, couch:get_json(project, R, S)},
    {<<"doctype_info">>, couch:get_json(doctype, R, S)}|Json
  ],
  
  {ok, Html} = document_edit_dtl:render(Vals),
  Html.

html_index(R, S) ->
  Doctype = wrq:path_info(doctype, R),
  Limit = wrq:get_qs_value("limit", R),
  
 {ok, Json} = case wrq:get_qs_value("query", R) of
    undefined -> couch:get_view_json(sortkeys, Doctype, "index", R, S);
    QueryId -> utils:get_query(QueryId, R, S) 
  end,
  
  Index = utils:add_encoded_keys(Json),
  
  Vals = [
    {<<"limit">>, Limit},
    {<<"title">>, list_to_binary(Doctype ++ " Index")}, 
    {<<"project_info">>, couch:get_json(project, R, S)},
    {<<"doctype_info">>, couch:get_json(doctype, R, S)}|Index
  ],
  
  {ok, Html} = document_index_dtl:render(Vals),
  Html.

html_document(R, S) ->
  Doctype = wrq:path_info(doctype, R),
  Json = document:normalize(couch:get_json(id, R, S)),
  
  Vals = [
    {<<"title">>, list_to_binary(Doctype)}, 
    {<<"project_info">>, couch:get_json(project, R, S)},
    {<<"doctype_info">>, couch:get_json(doctype, R, S)}|Json
  ],
  
  {ok, Html} = document_view_dtl:render(Vals),
  Html.
      
validate_authentication(Props, R, S) ->
  Project = couch:get_json(project, R, S),
  Name = jsn:get_value(<<"name">>, Project),
  ValidRoles = [<<"_admin">>, <<"manager">>, Name],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, R, S};
    false -> {proplists:get_value(auth_head, S), R, S}
  end.
