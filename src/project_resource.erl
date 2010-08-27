%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 author.
%% @doc This is the primary webmachine resource for the project.

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

init([]) -> {ok, []}.

resource_exists(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  DatabaseUrl = ?COUCHDB ++ "projects/",
   
  Resp = case wrq:path_info(id, ReqData) of
    undefined -> 
      case ibrowse:send_req(DatabaseUrl, Headers, head) of
        {ok, "404", _, _} ->
          create_database(),
          ibrowse:send_req(DatabaseUrl, Headers, head);
        Otherwise -> Otherwise
      end;
    Id -> ibrowse:send_req(DatabaseUrl ++ Id, Headers, head)
  end,
  case Resp of
    {ok, "200", _, _} -> {true, ReqData, State};
    {ok, "404", _, _} -> {false, ReqData, State}
  end. 

is_authorized(ReqData, State) ->
  proxy_auth:is_authorized(ReqData, [{source_mod, ?MODULE}|State]).

allowed_methods(ReqData, State) ->
  case wrq:path_info(id, ReqData) of
    undefined -> {['HEAD', 'GET', 'POST'], ReqData, State};
    _Id -> {['HEAD', 'DELETE'], ReqData, State}
  end.
  
delete_resource(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  ProjUrl = ?COUCHDB ++ "projects/" ++ wrq:path_info(id, ReqData),
  
  {ok, "200", _, Body} = ibrowse:send_req(ProjUrl, Headers, get),
  
  {struct, JsonIn} = mochijson2:decode(Body),
  
  RevQs = "?rev=" ++ binary_to_list(proplists:get_value(<<"_rev">>, JsonIn)),
  ProjUrl1 = ProjUrl ++ RevQs,
  DatabaseUrl = ?ADMINDB ++ "project-" ++ wrq:path_info(id, ReqData),
  
  % TODO: it would be best to do this in a transaction but that is maybe
  %       not possible.
  case ibrowse:send_req(ProjUrl1, Headers, delete) of
    {ok, "200", _, _} -> 
       case ibrowse:send_req(DatabaseUrl, [], delete) of
         {ok, "200", _, _} -> {true, ReqData, State}
       end
  end.
  
post_is_create(ReqData, State) ->
  {true, ReqData, State}.

create_path(ReqData, State) ->
  case get_uuid(State) of
    {ok, Uuid} -> {Uuid, ReqData, State};
    _ -> {undefined, ReqData, State}
  end.

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
  
  {struct, JsonIn} = mochijson2:decode(wrq:req_body(ReqData)),
  JsonOut = iolist_to_binary(mochijson2:encode({struct, [{<<"_id">>, list_to_binary(wrq:disp_path(ReqData))}|JsonIn]})),
  case ibrowse:send_req(?ADMINDB ++ NewDb, [], put) of
    {ok, "201", _, _} ->
      {ok, "201", _, _} = ibrowse:send_req(?COUCHDB ++ "projects", Headers, post, JsonOut),
      {ok, "201", _, _} = ibrowse:send_req(?COUCHDB ++ NewDb, Headers, post, config_skel()),
      {ok, "201", _, _} = ibrowse:send_req(?ADMINDB ++ NewDb, [ContentType], post, config_design_skel())
  end,
  {false, ReqData, State}.

% Helpers

get_uuid(State) ->
  Headers = proplists:get_value(headers, State),
  
  case ibrowse:send_req(?COUCHDB ++ "_uuids", Headers, get) of
    {ok, "200", _, Json} ->
      {struct, [{_, [Uuid]}]} = mochijson2:decode(list_to_binary(Json)),
      {ok, binary_to_list(Uuid)};
    _ -> undefined
  end.

validate_authentication({struct, Props}, ReqData, State) ->
  ValidRoles = [<<"_admin">>, <<"manager">>],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, ReqData, State};
    false -> {proplists:get_value(auth_head, State), ReqData, State}
  end.

add_renders({struct, JsonStruct}) ->
  Rows = proplists:get_value(<<"rows">>, JsonStruct),
  Renderings = [render_row(Project) || {struct, Project} <- Rows],
  {struct, [{<<"renderings">>, Renderings}|JsonStruct]}.
  
render_row(Project) ->
  {ok, Rendering} = project_list_elements_dtl:render(Project),
  iolist_to_binary(Rendering).

config_skel() ->
  {ok, Bin} = file:read_file("./priv/json/config.json"),
  binary_to_list(Bin).

config_design_skel() ->
  {ok, Bin} = file:read_file("./priv/json/design_default.json"),
  binary_to_list(Bin).

project_design_skel() ->
  {ok, Bin} = file:read_file("./priv/json/design_project.json"),
  binary_to_list(Bin).

create_database() ->
  ContentType = {"Content-Type","application/json"},
  {ok, "201", _, _} = ibrowse:send_req(?ADMINDB ++ "projects", [], put),
  {ok, "201", _, _} = ibrowse:send_req(?ADMINDB ++ "projects", [ContentType], post, project_design_skel()).
