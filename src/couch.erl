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
%% @doc Application specific CouchDB utility and helper functions

-module(couch).

-export([
  create/4,
  delete/2,
  exists/3,
  get_json/3,
  get_view_json/4,
  get_design_rev/3,
  get_uuid/2,
  update/4,
  update/5
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

get_json(project, R, _S) ->
  Id = wrq:path_info(project, R) -- "project-",
  Url = ?ADMINDB ++ "projects/" ++ Id,
  get_json_helper(Url, []);
  
get_json(doctype, R, S) ->
  Headers = proplists:get_value(headers, S),
  DataBaseUrl = ?COUCHDB ++ wrq:path_info(project, R) ++ "/",
  Doctype = wrq:path_info(doctype, R),
  get_json_helper(DataBaseUrl ++ Doctype, Headers);
  
get_json(id, R, S) ->
  Headers = proplists:get_value(headers, S),
  DataBaseUrl = ?COUCHDB ++ wrq:path_info(project, R) ++ "/",
  Id = wrq:path_info(id, R),
  get_json_helper(DataBaseUrl ++ Id, Headers);
  
get_json(Id, R, S) ->
  Headers = proplists:get_value(headers, S),
  DataBaseUrl = ?COUCHDB ++ wrq:path_info(project, R) ++ "/",
  get_json_helper(DataBaseUrl ++ Id, Headers).

get_json_helper(Url, Headers) ->  
  {ok, "200", _, Json} = ibrowse:send_req(Url, Headers, get),
  jsn:decode(Json).

get_view_json(Id, Name, R, S) ->
  Headers = proplists:get_value(headers, S),
  Qs = get_qs(wrq:raw_path(R)),
  Url = ?COUCHDB ++ wrq:path_info(project, R) ++ "/",
  Path = "_design/" ++ Id ++ "/_view/" ++ Name,
  FullUrl = Url ++ Path ++ Qs,
  {ok, "200", _, Json} = ibrowse:send_req(FullUrl, Headers, get),
  jsn:decode(Json).

get_design_rev(Name, R, _S) ->
  Url = ?ADMINDB ++ wrq:path_info(project, R) ++ "/_design/" ++ Name,
  Json = get_json_helper(Url, []),
  jsn:get_value(<<"_rev">>, Json).

get_uuid(_R, S) ->
  Headers = proplists:get_value(headers, S),
  
  {ok, "200", _, Json} = ibrowse:send_req(?COUCHDB ++ "_uuids", Headers, get),
  
  [Uuid] = jsn:get_value(<<"uuids">>, jsn:decode(Json)),
  {ok, binary_to_list(Uuid)}.

delete(R, S) ->
  Id = wrq:path_info(id, R),
  Rev = wrq:get_qs_value("rev", R),
  Url = ?COUCHDB ++ wrq:path_info(project, R) ++ "/" ++ Id ++ "?rev=" ++ Rev,
  Headers = [{"Content-Type","application/json"}|proplists:get_value(headers, S)],
  case ibrowse:send_req(Url, Headers, delete) of
    {ok, "200", _, _} -> {ok, deleted};
    {ok, "409", _, _} -> {409, <<"Conflict">>}
  end.

create(doc, Json, R, S) ->
  Url = ?COUCHDB ++ wrq:path_info(project, R) ++ "/_design/doctypes/_update/stamp",
  Headers = [{"Content-Type","application/json"}|proplists:get_value(headers, S)],
  create(Url, Headers, Json);

create(design, Json, R, _S) ->
  Url = ?ADMINDB ++ wrq:path_info(project, R),
  Headers = [{"Content-Type","application/json"}],
  create(Url, Headers, Json).

create(Url, Headers, Json) ->
  case ibrowse:send_req(Url, Headers, post, Json) of
    {ok, "201", _, _} -> {ok, created};
    {ok, "403", _, Body} ->
      Resp = jsn:decode(Body),
      Message = jsn:get_value(<<"reason">>, Resp),
      {403, Message}
  end.

update(doc, Id, Json, R, S) ->
  Url = ?COUCHDB ++ wrq:path_info(project, R) ++ "/_design/doctypes/_update/stamp/" ++ Id,
  Headers = [{"Content-Type","application/json"}|proplists:get_value(headers, S)],
  update(Url, Headers, Json);

% TODO I may want to reconsider not forcing the app to pick the design revision
% before hand.  
update(design, Id, Json, R, S) ->
  Json1 = jsn:decode(Json),
  Rev = get_design_rev(wrq:path_info(id, R), R, S),
  Json2 = jsn:set_value(<<"_rev">>, Rev, Json1),
  Url = ?ADMINDB ++ wrq:path_info(project, R) ++ "/_design/" ++ Id,
  Headers = [{"Content-Type","application/json"}],
  update(Url, Headers, jsn:encode(Json2)).


update(bulk, Json, R, S) ->
  Url = ?COUCHDB ++ wrq:path_info(project, R) ++ "/_bulk_docs",
  Headers = [{"Content-Type","application/json"}|proplists:get_value(headers, S)],
  case ibrowse:send_req(Url, Headers, post, Json) of
    {ok, "201", _, Body} -> {ok, Body};
    {ok, "403", _, Body} ->
      Resp = jsn:decode(Body),
      Message = jsn:get_value(<<"reason">>, Resp),
      {403, Message};
    {ok, "409", _, _} -> {409, <<"Conflict">>}
  end;
  
% TODO I may want to reconsider not forcing the app to pick the design revision
% before hand.  
update(design, Json, R, S) ->
  Json1 = jsn:decode(Json),
  Rev = get_design_rev(wrq:path_info(id, R), R, S),
  Json2 = jsn:set_value(<<"_rev">>, Rev, Json1),
  Url = ?ADMINDB ++ wrq:path_info(project, R) ++ "/_design/" ++ wrq:path_info(id, R),
  Headers = [{"Content-Type","application/json"}],
  update(Url, Headers, jsn:encode(Json2)).
  
update(Url, Headers, Json) ->
  case ibrowse:send_req(Url, Headers, put, Json) of
    {ok, "201", _, _} -> {ok, updated};
    {ok, "403", _, Body} ->
      Resp = jsn:decode(Body),
      Message = jsn:get_value(<<"reason">>, Resp),
      {403, Message};
    {ok, "409", _, _} -> {409, <<"Conflict">>}
  end.

exists(Target, R, S) ->
  Headers = proplists:get_value(headers, S),
  BaseUrl = ?COUCHDB ++ wrq:path_info(project, R) ++ "/",
  case ibrowse:send_req(BaseUrl ++ Target, Headers, head) of
    {ok, "200", _, _} -> true;
    {ok, "404", _, _} -> false
  end.

get_qs([]) ->
  [];  
get_qs([$?|Rest]) ->
  [$?|Rest];
get_qs([_|Rest]) ->
  get_qs(Rest).