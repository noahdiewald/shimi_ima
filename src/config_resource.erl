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
  is_authorized/2, 
  resource_exists/2,
  to_html/2,
  validate_authentication/3
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

init(Opts) -> {ok, Opts}.

to_html(R, S) ->
  User = proplists:get_value(user, S),
  Project = couch:get_json(project, R, S),
  {ok, DoctypesDesign} = design_doctypes_json_dtl:render(),
  Json = mochijson2:decode(DoctypesDesign),
  DoctypesRev = couch:get_design_rev("doctypes", R, S),
  Json1 = struct:set_value(<<"_rev">>, DoctypesRev, Json),
  Url = ?ADMINDB ++ wrq:path_info(project, R) ++ "/_design/doctypes",
  Headers = [{"Content-Type","application/json"}],
  {ok, "201", _, _} = ibrowse:send_req(Url, Headers, put, mochijson2:encode(Json1)),
  
  Vals = [
    {<<"user">>, User}
  ],
  
  {ok, Html} = config_dtl:render(struct:set_values(Vals, Project)),
  {Html, R, S}.

resource_exists(R, S) ->
  DatabaseUrl = ?ADMINDB ++ wrq:path_info(project, R),
  
  case ibrowse:send_req(DatabaseUrl, [], head) of
    {ok, "200", _, _} -> {true, R, S};
    {ok, "404", _, _} -> {false, R, S}
  end. 

is_authorized(R, S) ->
  proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

validate_authentication({struct, Props}, R, S) ->
  Project = couch:get_json(project, R, S),
  Name = struct:get_value(<<"name">>, Project),
  ValidRoles = [<<"_admin">>, <<"manager">>, Name],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, R, S};
    false -> {proplists:get_value(auth_head, S), R, S}
  end.
