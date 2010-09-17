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

resource_exists(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  BaseUrl = ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
  
  Doctype = wrq:path_info(doctype, ReqData),
  Id = wrq:path_info(id, ReqData),
   
  Resp = case proplists:get_value(target, State) of
    index -> ibrowse:send_req(BaseUrl ++ Doctype, Headers, head);
    identifier -> ibrowse:send_req(BaseUrl ++ Id, Headers, head)
  end,
  
  case Resp of
    {ok, "200", _, _} -> {true, ReqData, State};
    {ok, "404", _, _} -> {false, ReqData, State}
  end. 

is_authorized(ReqData, State) ->
  proxy_auth:is_authorized(ReqData, [{source_mod, ?MODULE}|State]).

allowed_methods(ReqData, State) ->
  case proplists:get_value(target, State) of
    index -> {['HEAD', 'GET', 'POST'], ReqData, State};
    identifier -> {['HEAD', 'GET'], ReqData, State}
  end.
  
post_is_create(ReqData, State) ->
  {true, ReqData, State}.

create_path(ReqData, State) ->
  Json = struct:from_json(wrq:req_body(ReqData)),
  
  {Id, Json1} = case struct:get_value(<<"_id">>, Json) of
    undefined -> 
      {ok, GenId} = couch_utils:get_uuid(ReqData, State),
      {GenId, struct:set_value(<<"_id">>, list_to_binary(GenId), Json)};
    IdBin -> {binary_to_list(IdBin), Json}
  end,
  
  Location = "http://" ++ wrq:get_req_header("host", ReqData) ++ "/" ++ wrq:path(ReqData) ++ "/" ++ Id,
  ReqData1 = wrq:set_resp_header("Location", Location, ReqData),
  
  {Id, ReqData1, [{posted_json, Json1}|State]}.

content_types_provided(ReqData, State) ->
  case proplists:get_value(target, State) of
    index -> {[{"application/json", to_json}], ReqData, State};
    identifier -> {[{"text/html", to_html}], ReqData, State}
  end.
  
content_types_accepted(ReqData, State) ->
  {[{"application/json", from_json}], ReqData, State}.
  
to_json(ReqData, State) ->
  Json = couch_utils:get_view_json(wrq:path_info(doctype, ReqData), "fieldsets", ReqData, State),
  JsonOut = struct:to_json(render_utils:add_renders(Json, config_fieldset_list_elements_dtl)),
  {JsonOut, ReqData, State}.
  
to_html(ReqData, State) ->
  Json = couch_util:get_json(id, ReqData, State),
  {ok, Html} = config_fieldset_dtl:render(Json),
  {Html, ReqData, State}.
  
from_json(ReqData, State) ->
  Json = proplists:get_value(posted_json, State),
  {ok, created} = couch_utils:create(doc, struct:to_json(Json), ReqData, State),
  
  % Create the fieldset's design document
  {ok, DesignJson} = design_fieldset_json_dtl:render(Json),
  {ok, created} = couch_utils:create(design, DesignJson, ReqData, State),
  
  {true, ReqData, State}.

% Helpers

validate_authentication({struct, Props}, ReqData, State) ->
  ValidRoles = [<<"_admin">>, <<"manager">>],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, ReqData, State};
    false -> {proplists:get_value(auth_head, State), ReqData, State}
  end.

