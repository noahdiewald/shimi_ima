%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(config_resource).
-export([
  init/1, 
  to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    {ok, Html} = config_dtl:render([{title, "Configuration"}]),
    {Html, ReqData, State}.
