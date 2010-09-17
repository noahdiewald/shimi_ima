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

-module(document_resource).

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

resource_exists(R, S) ->
  Headers = proplists:get_value(headers, S),
  BaseUrl = ?COUCHDB ++ wrq:path_info(project, R) ++ "/",
  
  Doctype = wrq:path_info(doctype, R),
  Id = wrq:path_info(id, R),
  
  Resp = case proplists:get_value(target, S) of
    index -> {ok, "200", [], []};
    identifier -> ibrowse:send_req(BaseUrl ++ Id, Headers, head);
    _ -> ibrowse:send_req(BaseUrl ++ Doctype, Headers, head)
  end,
  
  case Resp of
    {ok, "200", _, _} -> {true, R, S};
    {ok, "404", _, _} -> {false, R, S}
  end. 

is_authorized(R, S) ->
  proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
  case proplists:get_value(target, S) of
    doctype -> {['HEAD', 'GET', 'POST'], R, S};
    identifier -> {['HEAD', 'GET', 'PUT', 'DELETE'], R, S};
    _ -> {['HEAD', 'GET'], R, S}
  end.

content_types_provided(R, S) ->
  case wrq:path_info(id, R) of
    %undefined -> {[{"application/json", to_json}], R, S};
    _ -> {[{"text/html", to_html}], R, S}
  end.
  
to_html(R, S) ->
  case proplists:get_value(target, S) of
    index -> {html_index(R, S), R, S};
    new -> {html_new(R, S), R, S};
    doctype -> {html_documents(R, S), R, S};
    identifier -> {html_document(R, S), R, S}
  end.
  
% Helpers

html_index(R, S) ->
  ProjJson = couch:get_json(project, R, S),
  {struct, Json} = couch:get_view_json("doctypes", "all_simple", R, S),
  
  Properties = {struct, [
    {<<"title">>, <<"All Document Types">>}, 
    {<<"project_info">>, ProjJson}
  |Json]},
  
  {ok, Html} = doctype_index_dtl:render(Properties),
  Html.

html_new(R, S) ->
  Doctype = wrq:path_info(doctype, R),
  ProjJson = couch:get_json(project, R, S),
  DoctypeJson = couch:get_json(doctype, R, S),
  {struct, Json} = couch:get_view_json(Doctype, "fieldsets", R, S),
  
  Properties = {struct, [
    {<<"title">>, list_to_binary("New " ++ Doctype ++ " Documents")}, 
    {<<"project_info">>, ProjJson},
    {<<"doctype_info">>, DoctypeJson}
  |Json]},
  
  {ok, Html} = new_document_dtl:render(Properties),
  Html.

html_documents(R, S) ->
  Doctype = wrq:path_info(doctype, R),
  ProjJson = couch:get_json(project, R, S),
  DoctypeJson = couch:get_json(doctype, R, S),
  {struct, Json} = couch:get_view_json(Doctype, "alldocs", R, S),
  
  Properties = {struct, [
    {<<"title">>, list_to_binary("All " ++ Doctype ++ " Documents")}, 
    {<<"project_info">>, ProjJson},
    {<<"doctype_info">>, DoctypeJson}
  |Json]},
  
  {ok, Html} = doctype_dtl:render(Properties),
  Html.

html_document(_R, _S) ->
  "Document". 
    
validate_authentication({struct, Props}, R, S) ->
  ValidRoles = [<<"_admin">>, <<"manager">>],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, R, S};
    false -> {proplists:get_value(auth_head, S), R, S}
  end.
