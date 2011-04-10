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

-module(query_resource).

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
  Id = wrq:path_info(id, R),
  
  case proplists:get_value(target, S) of
    identifier -> {couch:exists(Id, R, S), R, S};
    view -> {couch:exists(Id, R, S), R, S};
    _ -> {true, R, S}
  end. 

is_authorized(R, S) ->
  proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
  case proplists:get_value(target, S) of
    index -> {['HEAD', 'GET', 'POST'], R, S};
    view -> {['HEAD', 'GET'], R, S};
    condition -> {['HEAD', 'GET'], R, S};
    identifier -> {['HEAD', 'GET', 'PUT', 'DELETE'], R, S}
  end.
  
delete_resource(R, S) ->
  case couch:delete(R, S) of
    {ok, deleted} -> {true, R, S};
    {409, _} ->
      Message = jsn:encode([{<<"message">>, <<"This query has been edited or deleted by another user.">>}]),
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 409}, R1, S}
  end.
  
post_is_create(R, S) ->
  {true, R, S}.

create_path(R, S) ->
  Json = jsn:decode(wrq:req_body(R)),
  
  {ok, Id} = couch:get_uuid(R, S),
  Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
  
  Location = "http://" ++ wrq:get_req_header("host", R) ++ "/" ++ wrq:path(R) ++ "/" ++ Id,
  R1 = wrq:set_resp_header("Location", Location, R),
  
  {Id, R1, [{posted_json, Json1}|S]}.

content_types_provided(R, S) ->
  {[{"text/html", to_html}], R, S}.
  
content_types_accepted(R, S) ->
  {[{"application/json", from_json}], R, S}.
  
to_html(R, S) ->
  case proplists:get_value(target, S) of
    view -> {html_view(R, S), R, S};
    index -> {html_index(R, S), R, S};
    condition -> {html_condition(R, S), R, S};
    identifier -> {html_identifier(R, S), R, S}
  end.
  
from_json(R, S) ->
  case proplists:get_value(target, S) of
    index -> json_create(R, S);
    identifier -> json_update(R, S)
  end.
  
% Helpers
  
json_create(R, S) ->
  Json = proplists:get_value(posted_json, S),
  case couch:create(doc, jsn:encode(Json), R, S) of
    {ok, created} -> {true, R, S};
    {403, Message} ->
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 403}, R1, S}
  end.
  
json_update(R, S) ->
  Json = jsn:decode(wrq:req_body(R)),
  Id = wrq:path_info(id, R),
  Rev = wrq:get_qs_value("rev", R),
  Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
  Json2 = jsn:set_value(<<"_rev">>, list_to_binary(Rev), Json1),
  
  case couch:update(doc, Id, jsn:encode(Json2), R, S) of
    {ok, updated} -> 
      {ok, _} = update_design(Json2, R, S),
      {true, R, S};
    {403, Message} ->
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 403}, R1, S};
    {409, _} ->
      Message = jsn:encode([{<<"message">>, <<"This document has been edited or deleted by another user.">>}]),
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 409}, R1, S}
  end.

update_design(Json, R, S) ->
  Expression = conditions:trans(jsn:get_value(<<"conditions">>, Json)),
  {ok, Design} = design_query_json_dtl:render([{<<"expression">>, Expression}|Json]),
  Id = "_design/" ++ wrq:path_info(id, R),
  
  case couch:exists(Id, R, S) of
    false ->
      couch:create(design, Design, R, S);
    _ ->
      couch:update(design, Design, R, S)
  end.
  
html_index(R, S) ->
  case couch:exists("_design/queries", R, S) of
    false -> 
      {ok, VJson} = design_queries_json_dtl:render(),
      {ok, created} = couch:create(design, VJson, R, S);
    _ -> undefined
  end,
  
  
  case wrq:get_qs_value("as", R) of
    "options" -> 
      Json = couch:get_view_json("queries", "options", R, S),
      {ok, Html} = options_dtl:render(Json);
    _Else ->
      Json = couch:get_view_json("queries", "all_simple", R, S),
      {ok, Html} = query_index_dtl:render(Json)
  end,
  
  Html.

html_identifier(R, S) ->
  Json = couch:get_json(id, R, S),
  Conditions = jsn:get_value(<<"conditions">>, Json),
  
  F = fun(X) -> 
    render_conditions(jsn, get_value, X, R, S)
  end,
  
  RenderedConditions = lists:map(F, Conditions),
  
  Vals = [
    {<<"rendered_conditions">>, RenderedConditions},
    {<<"fieldset_label">>, get_label(jsn:get_value(<<"fieldset">>, Json), R, S)},
    {<<"field_label">>, get_label(jsn:get_value(<<"field">>, Json), R, S)}
  |Json],
  
  {ok, Html} = query_edit_dtl:render(Vals),
  Html.

html_condition(R, S) ->
  render_conditions(wrq, get_qs_value, R, R, S).
  
html_view(R, S) ->
  QueryId = wrq:path_info(id, R),
  Limit = wrq:get_qs_value("limit", R),
  Json = couch:get_view_json(QueryId, "index", R, S),
  Vals = [{<<"limit">>, Limit}|Json],
  {ok, Html} = query_view_dtl:render(Vals),
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

render_conditions(Module, Function, Arg, R, S) ->
  {ok, Html} = case Module:Function("is_or", Arg) of
    true -> 
      query_condition_dtl:render([{<<"is_or">>, true}]);
    "true" -> 
      query_condition_dtl:render([{<<"is_or">>, true}]);
    _ ->
      Vals = [
        {<<"is_or">>, false},
        {<<"negate">>, Module:Function("negate", Arg) == true},
        {<<"fieldset">>, Module:Function("fieldset", Arg)},
        {<<"field">>, Module:Function("field", Arg)},
        {<<"operator">>, Module:Function("operator", Arg)},
        {<<"argument">>, Module:Function("argument", Arg)},
        {<<"fieldset_label">>, get_label(Module:Function("fieldset", Arg), R, S)},
        {<<"field_label">>, get_label(Module:Function("field", Arg), R, S)}],
      query_condition_dtl:render(Vals)
  end,
  
  Html.

get_label(Id, R, S) ->
  Json = couch:get_json(Id, R, S),
  jsn:get_value(<<"label">>, Json).