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
%% @doc Currently focused on simply rendering the configuration page.

-module(config_resource).
-export([
  init/1, 
  resource_exists/2,
  to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

init(Opts) -> {ok, Opts}.

to_html(R, S) ->
  Json = couch:get_json(project, R, S),
  
  {ok, Html} = config_dtl:render(Json),
  {Html, R, S}.

resource_exists(R, S) ->
  DatabaseUrl = ?ADMINDB ++ wrq:path_info(project, R),
  
  case ibrowse:send_req(DatabaseUrl, [], head) of
    {ok, "200", _, _} -> {true, R, S};
    {ok, "404", _, _} -> {false, R, S}
  end. 
