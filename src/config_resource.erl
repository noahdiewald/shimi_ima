%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 author.
%% @doc Currently focused on simply rendering the configuration page.

-module(config_resource).
-export([
  init/1, 
  to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    {ok, Html} = config_dtl:render([{title, "Configuration"},{project, wrq:path_info(project, ReqData)}]),
    {Html, ReqData, State}.
