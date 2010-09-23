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
  content_types_accepted/2,
  content_types_provided/2,
  create_path/2,
  from_json/2,
  init/1, 
  is_authorized/2,
  post_is_create/2,
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
  Doctype = wrq:path_info(doctype, R),
  Id = wrq:path_info(id, R),
  
  case proplists:get_value(target, S) of
    identifier -> {couch:exists(Id, R, S), R, S};
    _ -> {couch:exists(Doctype, R, S), R, S}
  end. 

is_authorized(R, S) ->
  proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
  case proplists:get_value(target, S) of
    index -> {['HEAD', 'GET', 'POST'], R, S};
    identifier -> {['HEAD', 'GET', 'PUT', 'DELETE'], R, S};
    new -> {['HEAD', 'GET'], R, S}
  end.
  
post_is_create(R, S) ->
  {true, R, S}.

create_path(R, S) ->
  Json = struct:from_json(wrq:req_body(R)),
  
  {ok, Id} = couch:get_uuid(R, S),
  Json1 = struct:set_value(<<"_id">>, list_to_binary(Id), Json),
  
  Location = "http://" ++ wrq:get_req_header("host", R) ++ "/" ++ wrq:path(R) ++ "/" ++ Id,
  R1 = wrq:set_resp_header("Location", Location, R),
  
  {Id, R1, [{posted_json, Json1}|S]}.

content_types_provided(R, S) ->
  {[{"text/html", to_html}], R, S}.
  
content_types_accepted(R, S) ->
  {[{"application/json", from_json}], R, S}.
  
to_html(R, S) ->
  case proplists:get_value(target, S) of
    new -> {html_new(R, S), R, S};
    index -> {html_documents(R, S), R, S};
    identifier -> {html_document(R, S), R, S}
  end.
  
from_json(R, S) ->
  Json = proplists:get_value(posted_json, S),
  {ok, created} = couch:create(doc, struct:to_json(Json), R, S),
  
  % Create the document's design document
  %{ok, DesignJson} = design_fieldset_json_dtl:render(Json),
  %{ok, created} = couch:create(design, DesignJson, R, S),
  
  {true, R, S}.
  
% Helpers

html_new(R, S) ->
  Doctype = wrq:path_info(doctype, R),
  Json = couch:get_view_json(Doctype, "fieldsets", R, S),
  
  Vals = [
    {<<"title">>, list_to_binary("New " ++ Doctype ++ " Document")}, 
    {<<"project_info">>, couch:get_json(project, R, S)},
    {<<"doctype_info">>, couch:get_json(doctype, R, S)}
  ],
  
  {ok, Html} = document_new_dtl:render(struct:set_values(Vals, Json)),
  Html.

html_documents(R, S) ->
  Doctype = wrq:path_info(doctype, R),
  Json = couch:get_view_json(Doctype, "alldocs", R, S),
  
  Vals = [
    {<<"title">>, list_to_binary("All " ++ Doctype ++ " Documents")}, 
    {<<"project_info">>, couch:get_json(project, R, S)},
    {<<"doctype_info">>, couch:get_json(doctype, R, S)}
  ],
  
  {ok, Html} = document_index_dtl:render(struct:set_values(Vals, Json)),
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
