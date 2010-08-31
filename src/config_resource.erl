%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 author.
%% @doc Currently focused on simply rendering the configuration page.

-module(config_resource).
-export([
  init/1, 
  resource_exists/2,
  to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
  Id = wrq:path_info(project, ReqData) -- "project-",
  ProjectUrl = ?ADMINDB ++ "projects/" ++ Id,
  
  {ok, "200", _, JsonIn} = ibrowse:send_req(ProjectUrl, [], get),
  {struct, Json} = mochijson2:decode(JsonIn),
  
  {ok, Html} = config_dtl:render([{title, "Configuration"}, {project, wrq:path_info(project, ReqData)}, {project_info, Json}]),
  {Html, ReqData, State}.

resource_exists(ReqData, State) ->
  DatabaseUrl = ?ADMINDB ++ wrq:path_info(project, ReqData),
  
  case ibrowse:send_req(DatabaseUrl, [], head) of
    {ok, "200", _, _} -> {true, ReqData, State};
    {ok, "404", _, _} -> {false, ReqData, State}
  end. 
