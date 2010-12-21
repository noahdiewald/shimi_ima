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
  to_html/2,
  touch_all/2
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
  
  case proplists:get_value(target, S) of
    index -> {couch:exists([], R, S), R, S};
    touch -> {couch:exists(Doctype, R, S), R, S}
  end.

is_authorized(R, S) ->
  proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
  case proplists:get_value(target, S) of
    index -> {['HEAD', 'GET'], R, S};
    touch -> {['HEAD', 'GET'], R, S}
  end.

content_types_provided(R, S) ->
  case proplists:get_value(target, S) of
    index -> {[{"text/html", to_html}], R, S};
    touch -> {[{"application/json", touch_all}], R, S}
  end.
  
to_html(R, S) ->
  {html_index(R, S), R, S}.
  
touch_all(R, S) ->
  DoctypeId = wrq:path_info(doctype, R),
  {ok, Template} = generate_template(DoctypeId, R, S),
  {ok, bulk_template} = erlydtl:compile(iolist_to_binary(Template), bulk_template),
  Documents = get_documents(DoctypeId, R, S),
  {ok, Rendering} = bulk_template:render([{<<"documents">>, Documents}]),
  
  case couch:update(bulk, Rendering, R, S) of
    {ok, Body} -> {Body, R, S};
    {403, Message} ->
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 403}, R1, S};
    {409, _} ->
      Message = jsn:encode([{<<"message">>, <<"This document has been edited or deleted by another user.">>}]),
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 409}, R1, S}
  end.
  
% Helpers

get_documents(DoctypeId, R, S) ->
  Documents1 = jsn:get_value(<<"rows">>, couch:get_view_json(DoctypeId, "ids_as_keys", R, S)),
  [jsn:get_value(<<"value">>,X) || X <- Documents1].

generate_template(DoctypeId, R, S) ->
  Doctype1 = couch:get_json(doctype, R, S),
  Fieldsets = get_fieldsets(DoctypeId, R, S),
  Doctype2 = jsn:set_value(<<"fieldsets">>, Fieldsets, Doctype1),
  batch_document_dtl:render([{<<"doctype">>, Doctype2}, {<<"ov">>, <<"{{">>}, {<<"cv">>, <<"}}">>}, {<<"ot">>, <<"{%">>}, {<<"ct">>, <<"%}">>}]).

get_fieldsets(DoctypeId, R, S) ->
  Fieldsets1 = jsn:get_value(<<"rows">>, couch:get_view_json(DoctypeId, "fieldsets", R, S)),
  Fieldsets2 = [jsn:get_value(<<"value">>,X) || X <- Fieldsets1],
  [add_fields(Fieldset, R, S) || Fieldset <- Fieldsets2].
  
add_fields(Fieldset, R, S) ->
  Id = binary_to_list(jsn:get_value(<<"_id">>, Fieldset)),
  Fields1 = jsn:get_value(<<"rows">>, couch:get_view_json(Id, "fields", R, S)),
  Fields2 = [jsn:get_value(<<"value">>,X) || X <- Fields1],
  jsn:set_value(<<"fields">>, Fields2, Fieldset).
  
html_index(R, S) ->
  User = proplists:get_value(user, S),
  ProjJson = couch:get_json(project, R, S),
  Json = couch:get_view_json("doctypes", "all_simple", R, S),
  
  Json1 = jsn:set_value(<<"title">>, <<"All Document Types">>, Json),
  Json2 = jsn:set_value(<<"project_info">>, ProjJson, Json1),
  Json3 = jsn:set_value(<<"user">>, User, Json2),
  
  {ok, Html} = doctype_index_dtl:render(Json3),
  Html.

validate_authentication(Props, R, S) ->
  Project = couch:get_json(project, R, S),
  Name = jsn:get_value(<<"name">>, Project),
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
