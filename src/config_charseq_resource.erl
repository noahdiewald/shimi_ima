%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of dictionary_maker.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% dictionary_maker is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with dictionary_maker. If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2011 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc Charseq configuration resource

-module(config_charseq_resource).

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
  Id = wrq:path_info(id, R),
  case proplists:get_value(target, S) of
    identifier -> {couch:exists(Id, R, S), R, S};
    index -> {couch:exists([], R, S), R, S}
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
      Message = jsn:encode([{<<"message">>, <<"This charseq has been edited or deleted by another user.">>}]),
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 409}, R1, S}
  end.
  
post_is_create(R, S) ->
  {true, R, S}.

create_path(R, S) ->
  Json = jsn:decode(wrq:req_body(R)),
  
  Id = binary_to_list(jsn:get_value(<<"_id">>, Json)),
  
  Location = "http://" ++ wrq:get_req_header("host", R) ++ "/" ++ wrq:path(R) ++ "/" ++ Id,
  R1 = wrq:set_resp_header("Location", Location, R),
  
  {Id, R1, [{posted_json, Json}|S]}.

content_types_provided(R, S) ->
  case proplists:get_value(target, S) of
    index -> {[{"text/html", index_html}], R, S};
    identifier -> {[{"text/html", id_html}], R, S}
  end.
  
content_types_accepted(R, S) ->
  {[{"application/json", from_json}], R, S}.
  
index_html(R, S) ->
  Request = fun () -> couch:get_view_json("charseqs", "listing", R, S) end,
  Success = fun (Json) -> {render:renderings(Json, config_charseq_list_elements_dtl), R, S} end,
  utils:report_indexing_timeout(Request, Success, R, S).
  
id_html(R, S) ->
  Json = couch:get_json(id, R, S), 
  {ok, Html} = config_charseq_dtl:render(Json),
  
  {Html, R, S}.
  
from_json(R, S) ->
  case proplists:get_value(target, S) of
    index -> json_create(R, S);
    identifier -> json_update(R, S)
  end.

json_create(R, S) ->  
  Json = jsn:decode(wrq:req_body(R)),
  {ok, created} = couch:create(doc, wrq:req_body(R), R, S),
  
  {ok, DesignJson} = design_charseq_json_dtl:render(Json),
  {ok, created} = couch:create(design, DesignJson, R, S),
  
  {true, R, S}.

json_update(R, S) ->
  Json = jsn:decode(wrq:req_body(R)),
  Id = wrq:path_info(id, R),
  Rev = wrq:get_qs_value("rev", R),
  Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
  Json2 = jsn:set_value(<<"_rev">>, list_to_binary(Rev), Json1),
  
  case couch:update(doc, Id, jsn:encode(Json2), R, S) of
    {ok, updated} -> 
      {ok, DesignJson} = design_charseq_json_dtl:render(Json),
      {ok, _} = couch:update(design, DesignJson, R, S),
      {true, R, S};
    {403, Message} ->
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 403}, R1, S};
    {409, _} ->
      Message = jsn:encode([{<<"message">>, <<"This charseq has been edited or deleted by another user.">>}]),
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

