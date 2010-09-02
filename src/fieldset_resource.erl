%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 author.
%% @doc The resource used accessing and editing document types.

-module(fieldset_resource).

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
  BaseUrl = ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
   
  Resp = case {wrq:path_info(domain, ReqData), wrq:path_info(id, ReqData)} of
    {Domain, undefined} -> ibrowse:send_req(BaseUrl ++ Domain, Headers, head);
    {undefined, Id} -> ibrowse:send_req(BaseUrl ++ Id, Headers, head);
    _ -> {ok, "404", [], []}
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
  
  {ok, Id} = case proplists:get_value(<<"_id">>, JsonIn) of
    undefined -> get_uuid(State);
    IdBin -> {ok, binary_to_list(IdBin)}
  end,
  
  {Id, ReqData, [{posted_json, Json}|State]}.

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
  View = "/_design/" ++ wrq:path_info(domain, ReqData) ++ "/_view/fieldsets",
  
  {ok, "200", _, JsonIn} = ibrowse:send_req(DataBaseUrl ++ View, Headers, get),
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
      {ok, Html} = fieldset_config_dtl:render(Json),
      {Html, ReqData, State}
    end.
  
from_json(ReqData, State) ->
  ContentType = {"Content-Type","application/json"},
  Headers = [ContentType|proplists:get_value(headers, State)],
  DataBaseUrl = ?COUCHDB ++ wrq:path_info(project, ReqData),
  AdminUrl = ?ADMINDB ++ wrq:path_info(project, ReqData),
  
  {struct, JsonIn} = mochijson2:decode(wrq:req_body(ReqData)),
  
  PropsOut = case proplists:get_value(<<"_id">>, JsonIn) of
    undefined -> [{<<"_id">>, list_to_binary(wrq:disp_path(ReqData))}|JsonIn];
    _ -> JsonIn
  end,
  
  JsonOut = iolist_to_binary(mochijson2:encode({struct, PropsOut})),
  
  % Create the fieldset
  {ok, "201", _, _} = ibrowse:send_req(DataBaseUrl, Headers, post, JsonOut),
  
  % Create the fieldset's design document
  {ok, DesignJson} = design_fieldset_json_dtl:render(PropsOut),
  {ok, "201", _, _} = ibrowse:send_req(AdminUrl, [ContentType], post, DesignJson),
  
  {true, ReqData, State}.

% Helpers

validate_authentication({struct, Props}, ReqData, State) ->
  ValidRoles = [<<"_admin">>, <<"manager">>],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, ReqData, State};
    false -> {proplists:get_value(auth_head, State), ReqData, State}
  end.

add_renders({struct, JsonStruct}) ->
  Rows = proplists:get_value(<<"rows">>, JsonStruct),
  Renderings = [render_row(Row) || {struct, Row} <- Rows],
  {struct, [{<<"renderings">>, Renderings}|JsonStruct]}.
  
render_row(Row) ->
  {struct, Value} = proplists:get_value(<<"value">>, Row),
  {ok, Rendering} = fieldset_list_elements_dtl:render(Value),
  iolist_to_binary(Rendering).

get_uuid(State) ->
  Headers = proplists:get_value(headers, State),
  
  case ibrowse:send_req(?COUCHDB ++ "_uuids", Headers, get) of
    {ok, "200", _, Json} ->
      {struct, [{_, [Uuid]}]} = mochijson2:decode(list_to_binary(Json)),
      {ok, binary_to_list(Uuid)};
    _ -> undefined
  end.

