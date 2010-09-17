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
%% @doc The resource used accessing and editing fieldsets in a configuration
%%      context.

-module(config_fieldset_resource).

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
  to_html/2,
  to_json/2
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
    index -> {couch:exists(Doctype, R, S), R, S};
    identifier -> {couch:exists(Id, R, S), R, S}
  end. 

is_authorized(R, S) ->
  proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
  case proplists:get_value(target, S) of
    index -> {['HEAD', 'GET', 'POST'], R, S};
    identifier -> {['HEAD', 'GET'], R, S}
  end.
  
post_is_create(R, S) ->
  {true, R, S}.

create_path(R, S) ->
  Json = struct:from_json(wrq:req_body(R)),
  
  {Id, Json1} = case struct:get_value(<<"_id">>, Json) of
    undefined -> 
      {ok, GenId} = couch:get_uuid(R, S),
      {GenId, struct:set_value(<<"_id">>, list_to_binary(GenId), Json)};
    IdBin -> {binary_to_list(IdBin), Json}
  end,
  
  Location = "http://" ++ wrq:get_req_header("host", R) ++ "/" ++ wrq:path(R) ++ "/" ++ Id,
  R1 = wrq:set_resp_header("Location", Location, R),
  
  {Id, R1, [{posted_json, Json1}|S]}.

content_types_provided(R, S) ->
  case proplists:get_value(target, S) of
    index -> {[{"application/json", to_json}], R, S};
    identifier -> {[{"text/html", to_html}], R, S}
  end.
  
content_types_accepted(R, S) ->
  {[{"application/json", from_json}], R, S}.
  
to_json(R, S) ->
  Json = couch:get_view_json(wrq:path_info(doctype, R), "fieldsets", R, S),
  JsonOut = struct:to_json(render:add_renders(Json, config_fieldset_list_elements_dtl)),
  {JsonOut, R, S}.
  
to_html(R, S) ->
  Json = couch:get_json(id, R, S),
  {ok, Html} = config_fieldset_dtl:render(Json),
  {Html, R, S}.
  
from_json(R, S) ->
  Json = proplists:get_value(posted_json, S),
  {ok, created} = couch:create(doc, struct:to_json(Json), R, S),
  
  % Create the fieldset's design document
  {ok, DesignJson} = design_fieldset_json_dtl:render(Json),
  {ok, created} = couch:create(design, DesignJson, R, S),
  
  {true, R, S}.

% Helpers

validate_authentication({struct, Props}, R, S) ->
  ValidRoles = [<<"_admin">>, <<"manager">>],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, R, S};
    false -> {proplists:get_value(auth_head, S), R, S}
  end.

