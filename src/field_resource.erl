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
%% @doc The resource used accessing and editing fields.

-module(field_resource).

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

init([]) -> {ok, []}.

resource_exists(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  BaseUrl = ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
   
  Resp = case {wrq:path_info(domain, ReqData), wrq:path_info(id, ReqData)} of
    {Domain, undefined} -> ibrowse:send_req(BaseUrl ++ Domain, Headers, head);
    {undefined, Id} -> ibrowse:send_req(BaseUrl ++ Id, Headers, head);
    _ -> {ok, "404", [], []}
  end,
  case Resp of
    {ok, "200", _, _} -> {true, ReqData, State};
    {ok, "404", _, _} -> {false, ReqData, State}
  end. 

is_authorized(ReqData, State) ->
  proxy_auth:is_authorized(ReqData, [{source_mod, ?MODULE}|State]).

allowed_methods(ReqData, State) ->
  case wrq:path_info(id, ReqData) of
    undefined -> {['HEAD', 'GET', 'POST'], ReqData, State};
    _Id -> {['HEAD', 'GET'], ReqData, State}
  end.
  
post_is_create(ReqData, State) ->
  {true, ReqData, State}.

create_path(ReqData, State) ->
  Json = mochijson2:decode(wrq:req_body(ReqData)),
  {struct, JsonIn} = Json,
  
  {ok, Id} = case proplists:get_value(<<"_id">>, JsonIn) of
    undefined -> get_uuid(State);
    IdBin -> {ok, binary_to_list(IdBin)}
  end,
  
  Location = "http://" ++ wrq:get_req_header("host", ReqData) ++ "/" ++ wrq:path(ReqData) ++ "/" ++ Id,
  ReqData1 = wrq:set_resp_header("Location", Location, ReqData),
  
  {Id, ReqData1, [{posted_json, Json}|State]}.

content_types_provided(ReqData, State) ->
  case wrq:path_info(id, ReqData) of
    undefined -> {[{"application/json", to_json}], ReqData, State};
    _ -> {[{"text/html", to_html}], ReqData, State}
  end.
  
content_types_accepted(ReqData, State) ->
  {[{"application/json", from_json}], ReqData, State}.
  
to_json(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  DataBaseUrl =  ?COUCHDB ++ wrq:path_info(project, ReqData),
  View = "/_design/" ++ wrq:path_info(domain, ReqData) ++ "/_view/fields",
  
  {ok, "200", _, JsonIn} = ibrowse:send_req(DataBaseUrl ++ View, Headers, get),
  JsonStruct = mochijson2:decode(JsonIn),
  JsonOut = mochijson2:encode(add_renders(JsonStruct)),
  {JsonOut, ReqData, State}.
  
to_html(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  DataBaseUrl =  ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
  
  case wrq:path_info(id, ReqData) of
    undefined ->
      {"Hello!!!", ReqData, State};
    Id ->
      {ok, "200", _, JsonIn} = ibrowse:send_req(DataBaseUrl ++ Id, Headers, get),
      {struct, Json} = mochijson2:decode(JsonIn),
      {ok, Html} = field_config_dtl:render(Json),
      {Html, ReqData, State}
    end.
  
from_json(ReqData, State) ->
  ContentType = {"Content-Type","application/json"},
  Headers = [ContentType|proplists:get_value(headers, State)],
  DataBaseUrl = ?COUCHDB ++ wrq:path_info(project, ReqData),
  %AdminUrl = ?ADMINDB ++ wrq:path_info(project, ReqData),
  
  {struct, JsonIn} = mochijson2:decode(wrq:req_body(ReqData)),
  
  PropsOut = case proplists:get_value(<<"_id">>, JsonIn) of
    undefined -> [{<<"_id">>, list_to_binary(wrq:disp_path(ReqData))}|JsonIn];
    _ -> JsonIn
  end,
  
  JsonOut = iolist_to_binary(mochijson2:encode({struct, PropsOut})),
  
  % Create the field
  {ok, "201", _, _} = ibrowse:send_req(DataBaseUrl, Headers, post, JsonOut),
  
  % Create the field's design document
  %{ok, DesignJson} = design_fieldset_json_dtl:render(PropsOut),
  %{ok, "201", _, _} = ibrowse:send_req(AdminUrl, [ContentType], post, DesignJson),
  
  {true, ReqData, State}.

% Helpers

validate_authentication({struct, Props}, ReqData, State) ->
  ValidRoles = [<<"_admin">>, <<"manager">>],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, ReqData, State};
    false -> {proplists:get_value(auth_head, State), ReqData, State}
  end.

add_renders({struct, JsonStruct}) ->
  Rows = proplists:get_value(<<"rows">>, JsonStruct),
  Renderings = [render_row(Row) || {struct, Row} <- Rows],
  {struct, [{<<"renderings">>, Renderings}|JsonStruct]}.
  
render_row(Row) ->
  {struct, Value} = proplists:get_value(<<"value">>, Row),
  {ok, Rendering} = field_list_elements_dtl:render(Value),
  iolist_to_binary(Rendering).

get_uuid(State) ->
  Headers = proplists:get_value(headers, State),
  
  case ibrowse:send_req(?COUCHDB ++ "_uuids", Headers, get) of
    {ok, "200", _, Json} ->
      {struct, [{_, [Uuid]}]} = mochijson2:decode(list_to_binary(Json)),
      {ok, binary_to_list(Uuid)};
    _ -> undefined
  end.

