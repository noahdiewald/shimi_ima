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
%%% @doc Application specific CouchDB utility and helper functions

-module(couch).

-export([
         bulk_update/3,
         create/4,
         create/5,
         delete/2,
         exists/3,
         exists/4,
         get_json/3,
         get_json/4,
         get_view_json/4,
         get_view_json/5,
         get_design_rev/3,
         get_uuid/2,
         new_db/3,
         update/4,
         update/5,
         update/6
        ]).

-include_lib("config.hrl").
-include_lib("types.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% @doc Make a new database
new_db(DB, _R, _S) ->
    {ok, "201", _, _} = ibrowse:send_req(DB, [], put),
    {ok, newdb}.
  
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
  
get_json(rev, R, S) ->
    Headers = proplists:get_value(headers, S),
    Url = ?COUCHDB ++ wrq:path_info(project, R) ++ "/" ++ 
        wrq:path_info(id, R) ++ "?rev=" ++ wrq:get_qs_value("rev", R),
    get_json_helper(Url, Headers);
  
get_json(Id, R, S) ->
    Headers = proplists:get_value(headers, S),
    DataBaseUrl = ?COUCHDB ++ wrq:path_info(project, R) ++ "/",
    get_json_helper(DataBaseUrl ++ Id, Headers).
  
get_json(safer, Id, R, S) ->
    Headers = proplists:get_value(headers, S),
    DataBaseUrl = ?COUCHDB ++ wrq:path_info(project, R) ++ "/",
    get_json_helper(safer, DataBaseUrl ++ Id, Headers).

get_json_helper(Url, Headers) ->  
    {ok, "200", _, Json} = ibrowse:send_req(Url, Headers, get),
    jsn:decode(Json).

get_json_helper(safer, Url, Headers) ->  
    case ibrowse:send_req(Url, Headers, get) of
        {ok, "200", _, Json} -> jsn:decode(Json);
        {ok, "404", _, _} -> undefined
    end.

get_view_json_helper(Id, Name, Qs, R, S) ->
    Headers = proplists:get_value(headers, S),
    Url = ?COUCHDB ++ wrq:path_info(project, R) ++ "/",
    Path = "_design/" ++ Id ++ "/_view/" ++ Name,
    FullUrl = Url ++ Path ++ Qs,
    case ibrowse:send_req(FullUrl, Headers, get) of
        {ok, "200", _, Json} -> {ok, jsn:decode(Json)};
        {ok, "404", _, _} -> {error, not_found};
        {error, req_timedout} -> {error, req_timedout}
    end.

get_view_json(Id, Name, R, S) ->
    Qs = view:normalize_vq(R),
    get_view_json_helper(Id, Name, "?" ++ Qs, R, S).

get_view_json(noqs, Id, Name, R, S) ->
    get_view_json_helper(Id, Name, [], R, S);

get_view_json(sortkeys, Id, Name, R, S) ->
    Qs = view:normalize_plus_vq(Id, R, S),
    get_view_json_helper(Id, Name, "?" ++ Qs, R, S).


get_design_rev(Name, R, S) ->
    Id = case Name of
             [$_|_] -> Name;
             _Else -> "_design/" ++ Name
         end,
    Url = case proplists:get_value(db, S) of
              undefined -> ?ADMINDB ++ wrq:path_info(project, R) ++ "/" ++ Id;
              Db -> Db ++ "/" ++ Id
          end,
    Json = get_json_helper(Url, []),
    {jsn:get_value(<<"version">>, Json), jsn:get_value(<<"_rev">>, Json)}.

get_uuid(_R, S) ->
    Headers = proplists:get_value(headers, S),
    {ok, "200", _, Json} = ibrowse:send_req(?COUCHDB ++ "_uuids", Headers, get),
    [Uuid] = jsn:get_value(<<"uuids">>, jsn:decode(Json)),
    {ok, binary_to_list(Uuid)}.

delete(R, S) ->
    Id = wrq:path_info(id, R),
    Rev = wrq:get_qs_value("rev", R),
    Url = ?COUCHDB ++ wrq:path_info(project, R) ++ "/" ++ Id ++ "?rev=" ++ Rev,
    Headers = [{"Content-Type",
                "application/json"}|proplists:get_value(headers, S)],
    case ibrowse:send_req(Url, Headers, delete) of
        {ok, "200", _, _} -> {ok, deleted};
        {ok, "409", _, _} -> {409, <<"Conflict">>}
    end.

create(direct, Json, R, S) ->
    create(doc, Json, ?COUCHDB ++ wrq:path_info(project, R), R, S);

create(doc, Json, R, S) ->
    create(doc, Json, ?COUCHDB ++ wrq:path_info(project, R), R, S);

create(design, Json, R, S) ->
    create(design, Json, ?ADMINDB ++ wrq:path_info(project, R), R, S).

create(direct, Json, DB, _R, S) ->
    Headers = [{"Content-Type","application/json"}|proplists:get_value(headers, S)],
    create(DB, Headers, Json);

create(doc, Json, DB, _R, S) ->
    Url = DB ++ "/_design/doctypes/_update/stamp",
    Headers = [{"Content-Type","application/json"}|proplists:get_value(headers, S)],
    create(Url, Headers, Json);

create(design, Json, DB, _R, _S) ->
    Url = DB,
    Headers = [{"Content-Type","application/json"}],
    create(Url, Headers, Json).

create(Url, Headers, Json) ->
    case ibrowse:send_req(Url, Headers, post, jsn:encode(jsn:decode(Json))) of
        {ok, "201", _, _} -> {ok, created};
        {ok, "403", _, Body} ->
            Resp = jsn:decode(Body),
            Message = jsn:get_value(<<"reason">>, Resp),
            {403, Message}
    end.

bulk_update(Docs, R, S) ->
    Headers = [{"Content-Type","application/json"}|
               proplists:get_value(headers, S)],
    Url = ?COUCHDB ++ wrq:path_info(project, R) ++ "/_bulk_docs",
    {ok, _, _, Body} = ibrowse:send_req(Url, Headers, post, jsn:encode(Docs)),
    jsn:decode(Body).
  
update(doc, Id, Json, R, S) ->
    Url = ?COUCHDB ++ wrq:path_info(project, R) ++ 
        "/_design/doctypes/_update/stamp/" ++ Id,
    Headers = [{"Content-Type","application/json"}|proplists:get_value(headers, S)],
    update(Url, Headers, Json);

update(design, Id, Json, R, S) ->
    update(design, Id, Json, ?ADMINDB ++ wrq:path_info(project, R), R, S).

update(design, Id, Json, DB, R, S) ->
    Json1 = jsn:decode(Json),
    Version = jsn:get_value(<<"version">>, Json1),
    case get_design_rev(Id, R, S) of
        {Version, _} -> {ok, updated};
        {_, Rev} ->
            Json2 = jsn:set_value(<<"_rev">>, Rev, Json1),
            Url = DB ++ "/_design/" ++ Id,
            Headers = [{"Content-Type","application/json"}],
            update(Url, Headers, jsn:encode(Json2))
    end.
  
update(design, Json, R, S) ->
    update(design, wrq:path_info(id, R), Json, R, S);

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
    end.
  
update(Url, Headers, Json) ->
    case ibrowse:send_req(Url, Headers, put, jsn:encode(jsn:decode(Json))) of
        {ok, "201", _, _} -> {ok, updated};
        {ok, "403", _, Body} ->
            Resp = jsn:decode(Body),
            Message = jsn:get_value(<<"reason">>, Resp),
            {403, Message};
        {ok, "409", _, _} -> {409, <<"Conflict">>}
    end.

exists(Target, R, S) ->
    exists(Target, ?COUCHDB ++ wrq:path_info(project, R), R, S).

exists(Target, DB, _R, S) ->
    Headers = proplists:get_value(headers, S),
    BaseUrl = DB ++ "/",
    case ibrowse:send_req(BaseUrl ++ Target, Headers, head) of
        {ok, "200", _, _} -> true;
        {ok, "404", _, _} -> false
    end.
