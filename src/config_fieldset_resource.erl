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
  delete_resource/2,
  from_json/2,
  init/1, 
  is_authorized/2,
  post_is_create/2,
  resource_exists/2,
  id_html/2,
  index_html/2
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
    identifier -> {['HEAD', 'GET', 'PUT', 'DELETE'], R, S}
  end.
  
delete_resource(R, S) ->
  case couch:delete(R, S) of
    {ok, deleted} -> {true, R, S};
    {409, _} ->
      Message = jsn:encode([{<<"message">>, <<"This document has been edited or deleted by another user.">>}]),
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 409}, R1, S}
  end.
  
post_is_create(R, S) ->
  {true, R, S}.

create_path(R, S) ->
  Json = jsn:decode(wrq:req_body(R)),
  
  {Id, Json1} = case jsn:get_value(<<"_id">>, Json) of
    undefined -> 
      GenId = utils:uuid(),
      {GenId, jsn:set_value(<<"_id">>, list_to_binary(GenId), Json)};
    IdBin -> {binary_to_list(IdBin), Json}
  end,
  
  Location = "http://" ++ wrq:get_req_header("host", R) ++ "/" ++ wrq:path(R) ++ "/" ++ Id,
  R1 = wrq:set_resp_header("Location", Location, R),
  
  {Id, R1, [{posted_json, Json1}|S]}.

content_types_provided(R, S) ->
  case proplists:get_value(target, S) of
    index -> {[{"text/html", index_html}], R, S};
    identifier -> {[{"text/html", id_html}], R, S}
  end.
  
content_types_accepted(R, S) ->
  {[{"application/json", from_json}], R, S}.
  
index_html(R, S) ->
    DT = list_to_binary(wrq:path_info(doctype, R)),
    QS = view:to_string(view:from_list([{"startkey", [DT, <<"0">>]},
                                        {"endkey", [DT, <<"z">>]},
                                        {"include_docs", true}])),

    {ok, Json} = couch:get_view_json("fieldsets", "all", QS, R, S),
    Rows = jsn:get_value(<<"rows">>, Json),
    Fieldsets = fieldset:arrange(Rows),
    {render:renderings([{<<"rows">>, Fieldsets}], 
                       config_fieldset_list_elements_dtl), R, S}.

id_html(R, S) ->
  Json = couch:get_json(id, R, S),
  {ok, Html} = config_fieldset_dtl:render(Json),
  {Html, R, S}.
  
from_json(R, S) ->
  case proplists:get_value(target, S) of
    index -> json_create(R, S);
    identifier -> json_update(R, S)
  end.

json_create(R, S) ->  
  Json = proplists:get_value(posted_json, S),
  {ok, created} = couch:create(doc, jsn:encode(Json), R, S),
  
  % Create the fieldset's design document
  {ok, DesignJson} = design_fieldset_json_dtl:render(Json),
  {ok, created} = couch:create(design, DesignJson, R, S),
  
  {true, R, S}.
  
json_update(R, S) ->
  Json = jsn:decode(wrq:req_body(R)),
  Id = wrq:path_info(id, R),
  Rev = wrq:get_qs_value("rev", R),
  Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
  Json2 = jsn:set_value(<<"_rev">>, list_to_binary(Rev), Json1),
  
  case couch:update(doc, Id, jsn:encode(Json2), R, S) of
    {ok, updated} -> {true, R, S};
    {403, Message} ->
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 403}, R1, S};
    {409, _} ->
      Message = jsn:encode([{<<"message">>, <<"This document has been edited or deleted by another user.">>}]),
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 409}, R1, S}
  end.

% Helpers

validate_authentication(Props, R, S) ->
  ValidRoles = [<<"_admin">>, <<"manager">>],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, R, S};
    false -> {proplists:get_value(auth_head, S), R, S}
  end.

