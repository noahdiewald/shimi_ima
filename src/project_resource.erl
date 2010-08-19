%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 author.
%% @doc This is the primary webmachine resource for the project.

-module(project_resource).
-export([
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,
  create_path/2,
  from_json/2,
  init/1, 
  post_is_create/2,
  resource_exists/2,
  to_json/2
]).

-define(COUCHDB, "http://127.0.0.1:5984/").

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

resource_exists(ReqData, State) ->
  case ibrowse:send_req(?COUCHDB ++ "projects", [], head) of
    {ok, "200", _, _} -> {true, ReqData, State};
    {ok, "404", _, _} -> {false, ReqData, State}
  end. 
  
allowed_methods(ReqData, State) ->
  {['HEAD', 'GET', 'POST'], ReqData, State}.
  
post_is_create(ReqData, State) ->
  {true, ReqData, State}.

create_path(ReqData, State) ->
  case get_uuid() of
    {ok, Uuid} -> {Uuid, ReqData, State};
    _ -> {undefined, ReqData, State}
  end.

content_types_provided(ReqData, State) ->
  {[{"application/json", to_json}], ReqData, State}.
  
content_types_accepted(ReqData, State) ->
  {[{"application/json", from_json}], ReqData, State}.
  
to_json(ReqData, State) ->
  {ok, "200", _, Json} = ibrowse:send_req("http://127.0.0.1:5984/projects/_all_docs", [], get),
  {Json, ReqData, State}.
  
from_json(ReqData, State) ->
  {struct, JsonIn} = mochijson2:decode(wrq:req_body(ReqData)),
  JsonOut = iolist_to_binary(mochijson2:encode({struct, [{<<"_id">>, list_to_binary(wrq:disp_path(ReqData))}|JsonIn]})),
  {ok, "201", _, _} = ibrowse:send_req(?COUCHDB ++ "projects", [{"Content-Type","application/json"}], post, JsonOut),
  {true, ReqData, State}.

get_uuid() ->
  case ibrowse:send_req(?COUCHDB ++ "_uuids", [], get) of
    {ok, "200", _, Json} ->
      {struct, [{_, [Uuid]}]} = mochijson2:decode(list_to_binary(Json)),
      {ok, binary_to_list(Uuid)};
    _ -> undefined
  end.
