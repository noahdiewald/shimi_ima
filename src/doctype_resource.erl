%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 author.
%% @doc The resource used accessing and editing document types.

-module(doctype_resource).

% Webmachine API
-export([
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,
  create_path/2,
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
  DatabaseUrl = ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
   
  Resp = case wrq:path_info(id, ReqData) of
    undefined -> ibrowse:send_req(DatabaseUrl, Headers, head);
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
    _Id -> {['HEAD', 'GET'], ReqData, State}
  end.
  
post_is_create(ReqData, State) ->
  {true, ReqData, State}.

create_path(ReqData, State) ->
  Json = mochijson2:decode(wrq:req_body(ReqData)),
  {struct, JsonIn} = Json,
  
  Id = proplists:get_value(<<"_id">>, JsonIn),
  
  Location = "http://" ++ wrq:get_req_header("host", ReqData) ++ "/" ++ wrq:path(ReqData) ++ "/" ++ Id,
  ReqData1 = wrq:set_resp_header("Location", Location, ReqData),
  
  {binary_to_list(Id), ReqData1, [{posted_json, Json}|State]}.

content_types_provided(ReqData, State) ->
  case wrq:path_info(id, ReqData) of
    undefined -> {[{"application/json", to_json}], ReqData, State};
    _ -> {[{"text/html", to_html}], ReqData, State}
  end.
  
content_types_accepted(ReqData, State) ->
  {[{"application/json", from_json}], ReqData, State}.
  
to_json(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  DataBaseUrl =  ?COUCHDB ++ wrq:path_info(project, ReqData),
  
  {ok, "200", _, JsonIn} = ibrowse:send_req(DataBaseUrl ++ "/_design/doctypes/_view/all", Headers, get),
  JsonStruct = mochijson2:decode(JsonIn),
  JsonOut = mochijson2:encode(add_renders(JsonStruct)),
  {JsonOut, ReqData, State}.
  
to_html(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  DataBaseUrl =  ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
  
  case wrq:path_info(id, ReqData) of
    undefined ->
      {"Hello!!!", ReqData, State};
    Id ->
      {ok, "200", _, JsonIn} = ibrowse:send_req(DataBaseUrl ++ Id, Headers, get),
      {struct, Json} = mochijson2:decode(JsonIn),
      {ok, Html} = doctype_config_dtl:render(Json),
      {Html, ReqData, State}
    end.
  
from_json(ReqData, State) ->
  ContentType = {"Content-Type","application/json"},
  Headers = [ContentType|proplists:get_value(headers, State)],
  
  DataBaseUrl = ?COUCHDB ++ wrq:path_info(project, ReqData),
  AdminUrl = ?ADMINDB ++ wrq:path_info(project, ReqData),
  
  {struct, JsonIn} = get_doctype_body_json(ReqData),
  {_, Id} = get_doctype_id(JsonIn),
  
  FieldsetUrl = get_fieldset_url(Id, ReqData),
  
  {ok, created} = create_doctype(DataBaseUrl, Headers, post, wrq:req_body(ReqData)),
  
  {ok, DesignJson} = get_design_doc_body_json(JsonIn),
  {ok, created} = create_design_doc(AdminUrl, [ContentType], post, DesignJson),
  
  {ok, FieldsetJson} = get_fieldset_body_json(JsonIn),
  {ok, created} = create_default_fieldset(FieldsetUrl, Headers, post, FieldsetJson),
  
  {true, ReqData, State}.

% Helpers

get_doctype_body_json(ReqData) ->
  mochijson2:decode(wrq:req_body(ReqData)).
  
get_design_doc_body_json(Json) ->
  design_doctype_json_dtl:render(Json).
  
get_fieldset_body_json(Json) ->
  doctype_default_fieldset_json_dtl:render(Json).
  
get_doctype_id(Json) ->
  proplists:lookup(<<"_id">>, Json).
  
get_fieldset_url(Id, ReqData) ->
  ?LOCAL ++ "project/" ++ wrq:path_info(project, ReqData) ++ "/config/" ++ 
    binary_to_list(Id) ++ "/fieldsets".
  
create_doctype(Url, Headers, Method, Body) ->
  {ok, "201", _, _} = ibrowse:send_req(Url, Headers, Method, Body),
  {ok, created}.

create_design_doc(Url, Headers, Method, Body) ->
  {ok, "201", _, _} = ibrowse:send_req(Url, Headers, Method, Body),
  {ok, created}.
  
create_default_fieldset(Url, Headers, Method, Body) ->
  {ok, "201", _, _} = ibrowse:send_req(Url, Headers, Method, Body),
  {ok, created}.

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
  {ok, Rendering} = doctype_list_elements_dtl:render(Project),
  iolist_to_binary(Rendering).

