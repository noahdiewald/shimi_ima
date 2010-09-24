%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 University of Wisconsin Madison Board of Regents.
%% Copyright (c) 2010 University of Wisconsin Madison Board of Regents
%%
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
%% THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%% @doc The is the resource used for managing projects.

-module(project_resource).

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
  index_html/2,
  main_html/2
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
  Headers = proplists:get_value(headers, S),
  DatabaseUrl = ?COUCHDB ++ "projects/",
  
  Id = wrq:path_info(id, R),
  
  Resp = case proplists:get_value(target, S) of
    identifier -> ibrowse:send_req(DatabaseUrl ++ Id, Headers, head);
    _ -> 
      % Create the database if it doesn't exist
      case ibrowse:send_req(DatabaseUrl, Headers, head) of
        {ok, "404", _, _} ->
          create_database(),
          ibrowse:send_req(DatabaseUrl, Headers, head);
        Otherwise -> Otherwise
      end
  end,
   
  case Resp of
    {ok, "200", _, _} -> {true, R, S};
    {ok, "404", _, _} -> {false, R, S}
  end. 

is_authorized(R, S) ->
  proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
  case proplists:get_value(target, S) of
    index -> {['HEAD', 'GET', 'POST'], R, S};
    main -> {['HEAD', 'GET'], R, S};
    identifier -> {['HEAD', 'DELETE'], R, S}
  end.
  
delete_resource(R, S) ->
  Headers = proplists:get_value(headers, S),
  ProjUrl = ?COUCHDB ++ "projects/" ++ wrq:path_info(id, R),
  
  {ok, "200", _, Body} = ibrowse:send_req(ProjUrl, Headers, get),
  JsonIn = struct:from_json(Body),
  
  RevQs = "?rev=" ++ binary_to_list(struct:get_value(<<"_rev">>, JsonIn)),
  ProjUrl1 = ProjUrl ++ RevQs,
  DatabaseUrl = ?ADMINDB ++ "project-" ++ wrq:path_info(id, R),
  
  case ibrowse:send_req(ProjUrl1, Headers, delete) of
    {ok, "200", _, _} -> 
       case ibrowse:send_req(DatabaseUrl, [], delete) of
         {ok, "200", _, _} -> {true, R, S}
       end
  end.
  
post_is_create(R, S) ->
  {true, R, S}.

create_path(R, S) ->
  {ok, Uuid} = couch:get_uuid(R, S),
  Location = "http://" ++ wrq:get_req_header("host", R) ++ "/" ++ wrq:path(R) ++ "/" ++ Uuid,
  R1 = wrq:set_resp_header("Location", Location, R),
  {Uuid, R1, S}.

content_types_provided(R, S) ->
  case proplists:get_value(target, S) of
    main -> {[{"text/html", main_html}], R, S};
    index -> {[{"text/html", index_html}], R, S}
  end.
  
content_types_accepted(R, S) ->
  {[{"application/json", from_json}], R, S}.
  
index_html(R, S) ->
  Headers = proplists:get_value(headers, S),
  
  {ok, "200", _, JsonIn} = ibrowse:send_req(?COUCHDB ++ "projects/_design/projects/_view/all", Headers, get),
  JsonStruct = mochijson2:decode(JsonIn),
  
  {renderings(JsonStruct), R, S}.
  
main_html(R, S) ->
  {ok, Html} = projects_dtl:render([{title, "Projects"}]),
  {Html, R, S}.
  
from_json(R, S) ->
  ContentType = {"Content-Type","application/json"},
  Headers = [ContentType|proplists:get_value(headers, S)],
  NewDb = "project-" ++ wrq:disp_path(R),
  
  JsonIn = mochijson2:decode(wrq:req_body(R)),
  JsonIn1 = struct:set_value(<<"_id">>, list_to_binary(wrq:disp_path(R)), JsonIn),
  JsonOut = iolist_to_binary(mochijson2:encode(JsonIn1)),
  
  case ibrowse:send_req(?ADMINDB ++ NewDb, [], put) of
    {ok, "201", _, _} ->
      {ok, "201", _, _} = ibrowse:send_req(?COUCHDB ++ "projects", Headers, post, JsonOut),
      {ok, DoctypesDesign} = design_doctypes_json_dtl:render(),
      {ok, "201", _, _} = ibrowse:send_req(?ADMINDB ++ NewDb, [ContentType], post, iolist_to_binary(DoctypesDesign))
  end,
  {true, R, S}.

% Helpers

validate_authentication({struct, Props}, R, S) ->
  ValidRoles = [<<"_admin">>, <<"manager">>],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, R, S};
    false -> {proplists:get_value(auth_head, S), R, S}
  end.

renderings(Json) ->
  Rows = struct:get_value(<<"rows">>, Json),
  [render_row(Project) || {struct, Project} <- Rows].
  
render_row(Project) ->
  {ok, Rendering} = project_list_elements_dtl:render(Project),
  iolist_to_binary(Rendering).

create_database() ->
  ContentType = {"Content-Type","application/json"},
  {ok, "201", _, _} = ibrowse:send_req(?ADMINDB ++ "projects", [], put),
  {ok, ProjectDesign} = design_project_json_dtl:render(),
  {ok, "201", _, _} = ibrowse:send_req(?ADMINDB ++ "projects", [ContentType], post, iolist_to_binary(ProjectDesign)).
