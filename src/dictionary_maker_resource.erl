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
  Headers = proplists:get_value(headers, State),
  BaseUrl = ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
  
  Doctype = wrq:path_info(doctype, ReqData),
  Id = wrq:path_info(id, ReqData),
  
  Resp = case proplists:get_value(target, State) of
    index -> {ok, "200", [], []};
    identifier -> ibrowse:send_req(BaseUrl ++ Id, Headers, head);
    _ -> ibrowse:send_req(BaseUrl ++ Doctype, Headers, head)
  end,
  
  case Resp of
    {ok, "200", _, _} -> {true, ReqData, State};
    {ok, "404", _, _} -> {false, ReqData, State}
  end. 

is_authorized(ReqData, State) ->
  proxy_auth:is_authorized(ReqData, [{source_mod, ?MODULE}|State]).

allowed_methods(ReqData, State) ->
  case proplists:get_value(target, State) of
    doctype -> {['HEAD', 'GET', 'POST'], ReqData, State};
    identifier -> {['HEAD', 'GET', 'PUT', 'DELETE'], ReqData, State};
    _ -> {['HEAD', 'GET'], ReqData, State}
  end.

content_types_provided(ReqData, State) ->
  case wrq:path_info(id, ReqData) of
    %undefined -> {[{"application/json", to_json}], ReqData, State};
    _ -> {[{"text/html", to_html}], ReqData, State}
  end.
  
to_html(ReqData, State) ->
  case proplists:get_value(target, State) of
    index -> {html_index(ReqData, State), ReqData, State};
    new -> {html_new(ReqData, State), ReqData, State};
    doctype -> {html_documents(ReqData, State), ReqData, State};
    identifier -> {html_document(ReqData, State), ReqData, State}
  end.
  
% Helpers

get_json(project, ReqData, State) ->
  Id = wrq:path_info(project, ReqData) -- "project-",
  Url = ?ADMINDB ++ "projects/" ++ Id,
  get_json_helper(admin, Url, State);
  
get_json(doctype, ReqData, State) ->
  DataBaseUrl = ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
  Doctype = wrq:path_info(doctype, ReqData),
  get_json_helper(DataBaseUrl ++ Doctype, State).

get_json_helper(Url, State) ->  
  Headers = proplists:get_value(headers, State),
  {ok, "200", _, JsonIn} = ibrowse:send_req(Url, Headers, get),
  {struct, Json} = mochijson2:decode(JsonIn),
  Json.

get_json_helper(admin, Url, _State) ->  
  {ok, "200", _, JsonIn} = ibrowse:send_req(Url, [], get),
  {struct, Json} = mochijson2:decode(JsonIn),
  Json.

get_view_json(Id, Name, ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  Url = ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
  {ok, "200", _, JsonIn} = ibrowse:send_req(Url ++ "_design/" ++ Id ++ "/_view/" ++ Name, Headers, get),
  {struct, Json} = mochijson2:decode(JsonIn),
  Json.

html_index(ReqData, State) ->
  ProjJson = get_json(project, ReqData, State),
  Json = get_view_json("doctypes", "all_simple", ReqData, State),
  
  Properties = [
    {title, "All Document Types"}, 
    {rows, [Row || {struct, Row} <- proplists:get_value(<<"rows">>, Json)]},
    {project_info, ProjJson}
  ],
  
  {ok, Html} = doctype_index_dtl:render(Properties),
  Html.

get_values([{struct, Row}|[]]) ->
  {struct, Value} = proplists:get_value(<<"value">>, Row),
  [Value];
get_values([{struct, Row}|Rest]) ->
  {struct, Value} = proplists:get_value(<<"value">>, Row),
  [Value|get_values(Rest)].

html_new(ReqData, State) ->
  Doctype = wrq:path_info(doctype, ReqData),
  ProjJson = get_json(project, ReqData, State),
  DoctypeJson = get_json(doctype, ReqData, State),
  Json = get_view_json(Doctype, "fieldsets", ReqData, State),
  
  Values = get_values(proplists:get_value(<<"rows">>, Json)),
  
  Properties = [
    {title, "New " ++ Doctype ++ " Documents"}, 
    {fieldsets, Values},
    {project_info, ProjJson},
    {doctype_info, DoctypeJson}
  ],
  
  {ok, Html} = new_document_dtl:render(Properties),
  Html.

html_documents(ReqData, State) ->
  Doctype = wrq:path_info(doctype, ReqData),
  ProjJson = get_json(project, ReqData, State),
  DoctypeJson = get_json(doctype, ReqData, State),
  Json = get_view_json(Doctype, "alldocs", ReqData, State),
  
  Properties = [
    {title, "All " ++ Doctype ++ " Documents"}, 
    {rows, [Row || {struct, Row} <- proplists:get_value(<<"rows">>, Json)]},
    {project_info, ProjJson},
    {doctype_info, DoctypeJson}
  ],
  
  {ok, Html} = doctype_dtl:render(Properties),
  Html.

html_document(_ReqData, _State) ->
  "Document". 
    
validate_authentication({struct, Props}, ReqData, State) ->
  ValidRoles = [<<"_admin">>, <<"manager">>],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, ReqData, State};
    false -> {proplists:get_value(auth_head, State), ReqData, State}
  end.
