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
  to_html/2,
  to_json/2
]).

% Custom
-export([
  validate_authentication/3
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

% Standard webmachine functions

init(Opts) -> {ok, Opts}.

resource_exists(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  DatabaseUrl = ?COUCHDB ++ "projects/",
  
  Id = wrq:path_info(id, ReqData),
  
  Resp = case proplists:get_value(target, State) of
    index -> 
      % Create the database if it doesn't exist
      case ibrowse:send_req(DatabaseUrl, Headers, head) of
        {ok, "404", _, _} ->
          create_database(),
          ibrowse:send_req(DatabaseUrl, Headers, head);
        Otherwise -> Otherwise
      end;
    identifier -> ibrowse:send_req(DatabaseUrl ++ Id, Headers, head)
  end,
   
  case Resp of
    {ok, "200", _, _} -> {true, ReqData, State};
    {ok, "404", _, _} -> {false, ReqData, State}
  end. 

is_authorized(ReqData, State) ->
  proxy_auth:is_authorized(ReqData, [{source_mod, ?MODULE}|State]).

allowed_methods(ReqData, State) ->
  case proplists:get_value(target, State) of
    index -> {['HEAD', 'GET', 'POST'], ReqData, State};
    identifier -> {['HEAD', 'DELETE'], ReqData, State}
  end.
  
delete_resource(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  ProjUrl = ?COUCHDB ++ "projects/" ++ wrq:path_info(id, ReqData),
  
  {ok, "200", _, Body} = ibrowse:send_req(ProjUrl, Headers, get),
  JsonIn = struct:from_json(Body),
  
  RevQs = "?rev=" ++ binary_to_list(struct:get_value(<<"_rev">>, JsonIn)),
  ProjUrl1 = ProjUrl ++ RevQs,
  DatabaseUrl = ?ADMINDB ++ "project-" ++ wrq:path_info(id, ReqData),
  
  case ibrowse:send_req(ProjUrl1, Headers, delete) of
    {ok, "200", _, _} -> 
       case ibrowse:send_req(DatabaseUrl, [], delete) of
         {ok, "200", _, _} -> {true, ReqData, State}
       end
  end.
  
post_is_create(ReqData, State) ->
  {true, ReqData, State}.

create_path(ReqData, State) ->
  {ok, Uuid} = couch_utils:get_uuid(ReqData, State),
  Location = "http://" ++ wrq:get_req_header("host", ReqData) ++ "/" ++ wrq:path(ReqData) ++ "/" ++ Uuid,
  ReqData1 = wrq:set_resp_header("Location", Location, ReqData),
  {Uuid, ReqData1, State}.

content_types_provided(ReqData, State) ->
  {[
    {"application/json", to_json}, 
    {"text/html", to_html}
   ], ReqData, State}.
  
content_types_accepted(ReqData, State) ->
  {[{"application/json", from_json}], ReqData, State}.
  
to_json(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  
  {ok, "200", _, JsonIn} = ibrowse:send_req(?COUCHDB ++ "projects/_design/projects/_view/all", Headers, get),
  JsonStruct = mochijson2:decode(JsonIn),
  JsonOut = mochijson2:encode(add_renders(JsonStruct)),
  {JsonOut, ReqData, State}.
  
to_html(ReqData, State) ->
  {ok, Html} = projects_dtl:render([{title, "Projects"}]),
  {Html, ReqData, State}.
  
from_json(ReqData, State) ->
  ContentType = {"Content-Type","application/json"},
  Headers = [ContentType|proplists:get_value(headers, State)],
  NewDb = "project-" ++ wrq:disp_path(ReqData),
  
  JsonIn = mochijson2:decode(wrq:req_body(ReqData)),
  JsonIn1 = struct:set_value(<<"_id">>, list_to_binary(wrq:disp_path(ReqData)), JsonIn),
  JsonOut = iolist_to_binary(mochijson2:encode(JsonIn1)),
  
  case ibrowse:send_req(?ADMINDB ++ NewDb, [], put) of
    {ok, "201", _, _} ->
      {ok, "201", _, _} = ibrowse:send_req(?COUCHDB ++ "projects", Headers, post, JsonOut),
      {ok, DoctypesDesign} = design_doctypes_json_dtl:render(),
      {ok, "201", _, _} = ibrowse:send_req(?ADMINDB ++ NewDb, [ContentType], post, iolist_to_binary(DoctypesDesign))
  end,
  {true, ReqData, State}.

% Helpers

validate_authentication({struct, Props}, ReqData, State) ->
  ValidRoles = [<<"_admin">>, <<"manager">>],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, ReqData, State};
    false -> {proplists:get_value(auth_head, State), ReqData, State}
  end.

add_renders(Json) ->
  Rows = struct:get_value(<<"rows">>, Json),
  Renderings = [render_row(Project) || {struct, Project} <- Rows],
  struct:set_value(<<"renderings">>, Renderings, Json).
  
render_row(Project) ->
  {ok, Rendering} = project_list_elements_dtl:render(Project),
  iolist_to_binary(Rendering).

create_database() ->
  ContentType = {"Content-Type","application/json"},
  {ok, "201", _, _} = ibrowse:send_req(?ADMINDB ++ "projects", [], put),
  {ok, ProjectDesign} = design_project_json_dtl:render(),
  {ok, "201", _, _} = ibrowse:send_req(?ADMINDB ++ "projects", [ContentType], post, iolist_to_binary(ProjectDesign)).
