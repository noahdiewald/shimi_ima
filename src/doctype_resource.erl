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
%% @doc Dictionary Maker main resource 

-module(doctype_resource).

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

% Standard webmachine functions

init(Opts) -> {ok, Opts}.

resource_exists(R, S) ->
  {couch:exists([], R, S), R, S}.

is_authorized(R, S) ->
  proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
  {['HEAD', 'GET'], R, S}.

content_types_provided(R, S) ->
    {[{"text/html", to_html}], R, S}.
  
to_html(R, S) ->
  {html_index(R, S), R, S}.
  
% Helpers
  
html_index(R, S) ->
    User = proplists:get_value(user, S),
    Id = wrq:path_info(project, R) -- "project",
    ProjectData = couch:get(Id, "shimi_ima", S),

    {ok, Json} = q:doctypes(R, S),
  
    Vals = [{<<"title">>, <<"All Document Types">>},
            {<<"project_info">>, ProjectData},
            {<<"user">>, User},
            {<<"doctypes">>, jsn:get_value(<<"rows">>, Json)}],
  
    {ok, Html} = render:render(doctype_index_dtl, Vals),
    Html.

validate_authentication(Props, R, S) ->
    Id = wrq:path_info(project, R) -- "project",
    ProjectData = couch:get(Id, "shimi_ima", S),
    Name = jsn:get_value(<<"name">>, ProjectData),
    NormalRoles = [<<"_admin">>, <<"manager">>, Name],
    IsNormal = fun (Role) -> lists:member(Role, NormalRoles) end,
    IsAdmin = fun (Role) -> lists:member(Role, [<<"_admin">>]) end,
    Normal = lists:any(IsNormal, proplists:get_value(<<"roles">>, Props)),
    Admin = lists:any(IsAdmin, proplists:get_value(<<"roles">>, Props)),
    Target = proplists:get_value(target, S),
  
    case {Target, Normal, Admin} of
        {index, true, _} -> {true, R, S};
        {index, false, _} -> {proplists:get_value(auth_head, S), R, S};
        {touch, _, true} -> {true, R, S};
        {touch, _, false} -> {proplists:get_value(auth_head, S), R, S}
    end.
