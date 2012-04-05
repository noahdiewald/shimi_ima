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
%% @doc Renders fields and fieldsets given an identifier 

-module(fieldset_resource).

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
-include_lib("config.hrl").

% Standard webmachine functions

init(Opts) -> {ok, Opts}.

resource_exists(R, S) ->
  Doctype = wrq:path_info(doctype, R),
  Id = wrq:path_info(id, R),
  
  case proplists:get_value(target, S) of
    index -> {couch:exists(Doctype, R, S), R, S};
    identifier -> {couch:exists(Id, R, S), R, S}
  end. 

is_authorized(R, S) ->
  proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
  {['HEAD', 'GET'], R, S}.

content_types_provided(R, S) ->
  {[{"text/html", to_html}], R, S}.
  
to_html(R, S) ->
  case proplists:get_value(target, S) of
    identifier -> {html_fieldset(R, S), R, S};
    index -> {html_fieldsets(R, S), R, S}
  end.
  
% Helpers

html_fieldset(R, S) -> 
  Json = couch:get_json(id, R, S),
  
  Vals = [
    {<<"project_info">>, couch:get_json(project, R, S)},
    {<<"doctype_info">>, couch:get_json(doctype, R, S)}|Json
  ],
  
  {ok, Html} = fieldset_dtl:render(Vals),
  Html.
  
html_fieldsets(R, S) -> 
  Doctype = wrq:path_info(doctype, R),
  {ok, Json} = couch:get_view_json(Doctype, "fieldsets_simple", R, S),
  {ok, Html} = options_dtl:render(Json),
  Html.
    
validate_authentication(Props, R, S) ->
  Project = couch:get_json(project, R, S),
  Name = jsn:get_value(<<"name">>, Project),
  ValidRoles = [<<"_admin">>, <<"manager">>, Name],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, R, S};
    false -> {proplists:get_value(auth_head, S), R, S}
  end.
