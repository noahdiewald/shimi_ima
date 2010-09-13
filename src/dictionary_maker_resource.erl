%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 Noah Diewald.
%% @doc Dictionary Maker main resource 

-module(dictionary_maker_resource).

% Webmachine API
-export([
  allowed_methods/2,
  content_types_provided/2,
  init/1, 
  is_authorized/2,
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

resource_exists(ReqData, State) ->
  %Headers = proplists:get_value(headers, State),
  %DataBaseUrl = ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
  
  case proplists:get_value(index, State) of
    undefined -> {true, ReqData, State};
    _ -> {true, ReqData, State}
  end.

is_authorized(ReqData, State) ->
  proxy_auth:is_authorized(ReqData, [{source_mod, ?MODULE}|State]).

allowed_methods(ReqData, State) ->
  {['HEAD', 'GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
  case wrq:path_info(id, ReqData) of
    %undefined -> {[{"application/json", to_json}], ReqData, State};
    _ -> {[{"text/html", to_html}], ReqData, State}
  end.
  
to_html(ReqData, State) ->
  case proplists:get_value(index, State) of
    undefined -> {"<html><body>Hello, new world</body></html>", ReqData, State};
    true -> {html_index(ReqData, State), ReqData, State}
  end.
  
% Helpers

html_index(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  DataBaseUrl = ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
  Id = wrq:path_info(project, ReqData) -- "project-",
  ProjectUrl = ?ADMINDB ++ "projects/" ++ Id,
  
  {ok, "200", _, ProjJsonIn} = ibrowse:send_req(ProjectUrl, [], get),
  {struct, ProjJson} = mochijson2:decode(ProjJsonIn),
  
  {ok, "200", _, JsonIn} = ibrowse:send_req(DataBaseUrl ++ "/_design/doctypes/_view/all_simple", Headers, get),
  {struct, Json} = mochijson2:decode(JsonIn),
  Properties = [
    {title, "All Document Types"}, 
    {rows, [Row || {struct, Row} <- proplists:get_value(<<"rows">>, Json)]},
    {project_info, ProjJson}
  ],
  io:format("~p", [Properties]),
  {ok, Html} = doctype_index_dtl:render(Properties),
  Html.
   
validate_authentication({struct, Props}, ReqData, State) ->
  ValidRoles = [<<"_admin">>, <<"manager">>],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, ReqData, State};
    false -> {proplists:get_value(auth_head, State), ReqData, State}
  end.
