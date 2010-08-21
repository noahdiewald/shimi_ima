%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 author.
%% @doc This is the primary webmachine resource for the project.

-module(project_resource).
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
  to_json/2
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

% Standard webmachine functions

init([]) -> {ok, []}.

resource_exists(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  DatabaseUrl = ?COUCHDB ++ "projects/",
   
  Resp = case wrq:path_info(id, ReqData) of
    undefined -> ibrowse:send_req(DatabaseUrl, Headers, head);
    Id -> ibrowse:send_req(DatabaseUrl ++ Id, Headers, head)
  end,
  case Resp of
    {ok, "200", _, _} -> {true, ReqData, State};
    {ok, "404", _, _} -> {false, ReqData, State}
  end. 

is_authorized(ReqData, State) ->
  State1 = [{auth_head, "Basic realm=dictionary"}|State],
  case wrq:get_req_header("authorization", ReqData) of
    "Basic " ++ Base64 ->
      State2 = update_client_headers({"Authorization", "Basic " ++ Base64}, State1),
      do_basic_authentication(Base64, ReqData, State2);
    _ -> {proplists:get_value(auth_head, State1), ReqData, State1}
  end.

allowed_methods(ReqData, State) ->
  case wrq:path_info(id, ReqData) of
    undefined -> {['HEAD', 'GET', 'POST'], ReqData, State};
    _Id -> {['HEAD', 'DELETE'], ReqData, State}
  end.
  
delete_resource(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  ProjUrl = ?COUCHDB ++ "projects/" ++ wrq:path_info(id, ReqData),
  
  {ok, "200", _, Body} = ibrowse:send_req(ProjUrl, Headers, get),
  
  {struct, JsonIn} = mochijson2:decode(Body),
  
  RevQs = "?rev=" ++ binary_to_list(proplists:get_value(<<"_rev">>, JsonIn)),
  ProjUrl1 = ProjUrl ++ RevQs,
  DatabaseUrl = ?ADMINDB ++ "project-" ++ wrq:path_info(id, ReqData),
  
  % TODO: it would be best to do this in a transaction but that is maybe
  %       not possible.
  case ibrowse:send_req(ProjUrl1, Headers, delete) of
    {ok, "200", _, _} -> 
       case ibrowse:send_req(DatabaseUrl, [], delete) of
         {ok, "200", _, _} -> {true, ReqData, State}
       end
  end.
  
post_is_create(ReqData, State) ->
  {true, ReqData, State}.

create_path(ReqData, State) ->
  case get_uuid(State) of
    {ok, Uuid} -> {Uuid, ReqData, State};
    _ -> {undefined, ReqData, State}
  end.

content_types_provided(ReqData, State) ->
  {[{"application/json", to_json}], ReqData, State}.
  
content_types_accepted(ReqData, State) ->
  {[{"application/json", from_json}], ReqData, State}.
  
to_json(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  
  {ok, "200", _, Json} = ibrowse:send_req(?COUCHDB ++ "projects/_design/projects/_view/all", Headers, get),
  {Json, ReqData, State}.
  
from_json(ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  
  {struct, JsonIn} = mochijson2:decode(wrq:req_body(ReqData)),
  JsonOut = iolist_to_binary(mochijson2:encode({struct, [{<<"_id">>, list_to_binary(wrq:disp_path(ReqData))}|JsonIn]})),
  case ibrowse:send_req(?ADMINDB ++ "project-" ++ wrq:disp_path(ReqData), [], put) of
    {ok, "201", _, _} ->
      {ok, "201", _, _} = ibrowse:send_req(?COUCHDB ++ "projects", [{"Content-Type","application/json"}|Headers], post, JsonOut)
  end,
  {false, ReqData, State}.

% Helpers

get_uuid(State) ->
  Headers = proplists:get_value(headers, State),
  
  case ibrowse:send_req(?COUCHDB ++ "_uuids", Headers, get) of
    {ok, "200", _, Json} ->
      {struct, [{_, [Uuid]}]} = mochijson2:decode(list_to_binary(Json)),
      {ok, binary_to_list(Uuid)};
    _ -> undefined
  end.

update_client_headers(Header, State) ->
  case proplists:get_value(headers, State) of
    undefined -> [{headers, [Header]}|State];
    Headers -> 
      State1 = proplists:delete(headers, State),
      [{headers, [Header|Headers]}|State1]
    end.
    
do_basic_authentication(Base64, ReqData, State) ->
  Str = base64:mime_decode_to_string(Base64),
  case string:tokens(Str, ":") of
    [Username, Password] -> couchdb_authenticate(Username, Password, ReqData, State);
    _ -> {proplists:get_value(auth_head, State), ReqData, State}
  end.
  
couchdb_authenticate(Username, Password, ReqData, State) ->
  Body = "name=" ++ Username ++ "&password=" ++ Password,
  Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
  Resp = ibrowse:send_req(?COUCHDB ++ "_session", Headers, post, Body),
  case Resp of
    {ok, "200", _, Json} -> 
      validate_authentication(mochijson2:decode(Json), ReqData, State);
    {ok, "401", _, _} -> {proplists:get_value(auth_head, State), ReqData, State}
  end.

validate_authentication({struct, Props}, ReqData, State) ->
  ValidRoles = [<<"_admin">>, <<"manager">>],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, ReqData, State};
    false -> {proplists:get_value(auth_head, State), ReqData, State}
  end.
