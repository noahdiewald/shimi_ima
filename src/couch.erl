%%% Copyright 2012 University of Wisconsin Madison Board of Regents.
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
         fold_view/6,
         get_db_seq/1,
         get_dbs/0,
         get_design_rev/3,
         get_json/3,
         get_json/4,
         get_view_json/4,
         get_view_json/5,
         get_view_json/6,
         get_views/1,
         get_uuid/2,
         new_db/3,
         replicate/2,
         should_wait/2,
         update/4,
         update/5,
         update/6,
         update_doctype_version/2,
         user_list/0
        ]).

-include_lib("types.hrl").
-include_lib("webmachine/include/webmachine.hrl").

user_list() -> [].

replicate(Source, Target) ->
    Headers = [{"Content-Type", "application/json"}],
    Url = utils:adb() ++ "_replicate",
    Json = [{<<"source">>, list_to_binary(Source)},
            {<<"target">>, list_to_binary(Target)}],
    case ibrowse:send_req(Url, Headers, post, jsn:encode(Json)) of
        {ok, [$2|_], _, _} ->
            {ok, replicated};
        Otherwise ->
            Otherwise
    end.
    
%% @doc Make a new database
new_db(DB, _R, _S) ->
    {ok, "201", _, _} = ibrowse:send_req(DB, [], put),
    {ok, newdb}.

get(Id, R, S) ->  
    Headers = proplists:get_value(headers, S),
    DataBaseUrl = utils:ndb() ++ wrq:path_info(project, R) ++ "/",
    get_helper(DataBaseUrl ++ Id, Headers).
  
get_json(project, R, _S) ->
    Id = wrq:path_info(project, R) -- "project-",
    Url = utils:adb() ++ "projects/" ++ Id,
    get_json_helper(Url, []);
  
get_json(doctype, R, S) ->
    Headers = proplists:get_value(headers, S),
    DataBaseUrl = utils:ndb() ++ wrq:path_info(project, R) ++ "/",
    Doctype = wrq:path_info(doctype, R),
    get_json_helper(DataBaseUrl ++ Doctype, Headers);
  
get_json(id, R, S) ->
    Headers = proplists:get_value(headers, S),
    DataBaseUrl = utils:ndb() ++ wrq:path_info(project, R) ++ "/",
    Id = wrq:path_info(id, R),
    Url = DataBaseUrl ++ Id ++ "?revs_info=true",
    get_json_helper(Url, Headers);
  
get_json(rev, R, S) ->
    Revision = case wrq:get_qs_value("rev", R) of
              undefined ->
                  wrq:path_info(rev, R);
              Rev -> Rev
          end,
    Headers = proplists:get_value(headers, S),
    Url = utils:ndb() ++ wrq:path_info(project, R) ++ "/" ++ 
        wrq:path_info(id, R) ++ "?rev=" ++ Revision,
    get_json_helper(Url, Headers);
  
get_json(Id, R, S) ->
    Headers = proplists:get_value(headers, S),
    DataBaseUrl = utils:ndb() ++ wrq:path_info(project, R) ++ "/",
    get_json_helper(DataBaseUrl ++ Id, Headers).
    
get_json(safer, Id, R, S) ->
    Headers = proplists:get_value(headers, S),
    DataBaseUrl = utils:ndb() ++ wrq:path_info(project, R) ++ "/",
    get_json_helper(safer, DataBaseUrl ++ Id, Headers).

get_db_seq(Project) ->
    case get_db_info(Project) of
        {ok, Json} -> {ok, jsn:get_value(<<"update_seq">>, Json)};
        Otherwise -> Otherwise
    end.

get_dbs() ->
    Url = utils:adb() ++ "/projects/_all_docs?include_docs=true",
    get_json_helper(Url, []).

get_db_info(Project) ->
    Url = utils:adb() ++ Project,
    get_json_helper(safer, Url, []).
    
should_wait(Project, ViewPath) ->
    Url = utils:adb() ++ Project ++ "/" ++ ViewPath ++ "?limit=1",
    case ibrowse:send_req(Url, [], get) of
        {error, req_timedout} -> true;
        _ -> false
    end.

get_json_helper(Url, Headers) ->
    case get_helper(Url, Headers) of
        {ok, Json} -> jsn:decode(Json);
        Else -> Else
    end.

get_json_helper(safer, Url, Headers) ->  
    case get_helper(Url, Headers) of
        {ok, Json} -> {ok, jsn:decode(Json)};
        Else -> Else
    end.

get_helper(Url, Headers) ->
    case ibrowse:send_req(Url, Headers, get, [], 
                          [{connect_timeout, 500}, 
                           {inactivity_timeout, 10000}]) of
        {ok, "200", _, Json} -> {ok, Json};
        {ok, "404", _, _} -> {error, not_found};
        {error, req_timedout} -> {error, req_timedout}
    end.
    
get_view_json_helper(Id, Name, Qs, R, S) ->
    Headers = proplists:get_value(headers, S),
    Url = utils:ndb() ++ wrq:path_info(project, R) ++ "/",
    Path = "_design/" ++ Id ++ "/_view/" ++ Name,
    FullUrl = Url ++ Path ++ Qs,
    case get_json_helper(safer, FullUrl, Headers) of
        undefined -> {error, not_found};
        {error, req_timedout} -> {error, req_timedout};
        {error, not_found} -> {error, not_found};
        {ok, Json} -> {ok, Json}
    end.

fold_view(Id, Name, Qs, Fun, R, S) ->
    {ok, ViewJson} = get_view_json(Id, Name, Qs, R, S),
    Rows = jsn:get_value(<<"rows">>, ViewJson),
    lists:foldr(Fun, [], Rows).

get_view_json(Id, Name, R, S) ->
    Qs = view:normalize_vq(R),
    get_view_json(Id, Name, Qs, R, S).

get_view_json(noqs, Id, Name, R, S) ->
    get_view_json_helper(Id, Name, [], R, S);
get_view_json(sortkeys, Id, Name, R, S) ->
    Qs = view:normalize_sortkey_vq(Id, R, S),
    get_view_json_helper(Id, Name, "?" ++ Qs, R, S);
get_view_json(Id, Name, Qs, R, S) ->
    get_view_json_helper(Id, Name, "?" ++ Qs, R, S).

get_view_json(sortkeys, Id, Name, Doctype, R, S) ->
    Qs = view:normalize_sortkey_vq(Doctype, R, S),
    get_view_json_helper(Id, Name, "?" ++ Qs, R, S).

%% @doc Given a row from a query of all db design documents, find all
%% the view paths for a given design document.
-spec get_view_path(jsn:json_term()) -> [[binary()]].
get_view_path(Row) ->
    Id = proplists:get_value(<<"id">>, Row),
    Doc = proplists:get_value(<<"doc">>, Row),
    F = fun({ViewName, _}, Acc) ->
                [iolist_to_binary([Id, <<"/_view/">>, ViewName])|Acc]
        end,
    case proplists:get_value(<<"views">>, Doc) of
        undefined ->
            undefined;
        Views ->
            lists:foldl(F, [], Views)
    end.

get_views(Project) when is_binary(Project) ->
    get_views(binary_to_list(Project));
get_views(Project) when is_list(Project) ->
    Qs = view:to_string(view:from_list([{"startkey", <<"_design/">>},
                                        {"endkey", <<"_design0">>},
                                        {"include_docs", true}])),
    Url = utils:adb() ++ Project ++ "/" ++ "_all_docs" ++ "?" ++ Qs,
    Designs = proplists:get_value(<<"rows">>, get_json_helper(Url, [])),
    Filter = fun(X) -> 
                     case X of
                         undefined -> false;
                         _ -> true 
                     end
             end,
    lists:flatten(
      lists:filter(Filter, lists:map(fun get_view_path/1, Designs)));
get_views(R) when is_tuple(R) ->
    get_views(wrq:path_info(project, R)).

get_design_rev(Name, R, S) ->
    Id = case Name of
             [$_|_] -> Name;
             _Else -> "_design/" ++ Name
         end,
    Url = case proplists:get_value(db, S) of
              undefined -> utils:adb() ++ wrq:path_info(project, R) ++ "/" ++ Id;
              Db -> Db ++ "/" ++ Id
          end,
    case get_json_helper(safer, Url, []) of
        {ok, Json} ->
            {jsn:get_value(<<"version">>, Json), 
             jsn:get_value(<<"_rev">>, Json)};
        Else -> Else
    end.

% TODO: this once queried couchdb. Now the callers could just use the
% function directly.
get_uuid(_R, _S) ->
    utils:uuid().

delete(R, S) ->
    Id = wrq:path_info(id, R),
    Rev = wrq:get_qs_value("rev", R),
    Url = utils:ndb() ++ wrq:path_info(project, R) ++ "/" ++ Id ++ "?rev=" ++ Rev,
    Headers = [{"Content-Type",
                "application/json"}|proplists:get_value(headers, S)],
    case ibrowse:send_req(Url, Headers, delete) of
        {ok, "200", _, _} -> 
            case exists("_design/" ++ Id, R, S) of
                true ->
                    {_, DRev} = get_design_rev(Id, R, S),
                    DUrl = utils:ndb() ++ wrq:path_info(project, R) ++ 
                        "/_design/" ++ Id ++ "?rev=" ++ DRev,
                    {ok, "200", _, _} = ibrowse:send_req(DUrl, Headers, delete);
                false -> ok
            end,
            {ok, deleted};
        {ok, "409", _, _} -> {409, <<"Conflict">>}
    end.

create(direct, Json, R, S) ->
    create(doc, Json, utils:ndb() ++ wrq:path_info(project, R), R, S);

create(doc, Json, R, S) ->
    create(doc, Json, utils:ndb() ++ wrq:path_info(project, R), R, S);

create(design, Json, R, S) ->
    create(design, Json, utils:adb() ++ wrq:path_info(project, R), R, S).

create(direct, Json, DB, _R, S) ->
    Headers = [{"Content-Type","application/json"}|proplists:get_value(headers, S)],
    create(DB, Headers, Json);

create(doc, Json, DB, _R, S) ->
    Url = DB ++ "/_design/shimi_ima/_update/stamp",
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
    Url = utils:ndb() ++ wrq:path_info(project, R) ++ "/_bulk_docs",
    {ok, _, _, Body} = ibrowse:send_req(Url, Headers, post, jsn:encode(Docs)),
    jsn:decode(Body).
  
update(doc, Id, Json, R, S) ->
    Project =  wrq:path_info(project, R),
    Url = utils:ndb() ++ Project ++ "/_design/shimi_ima/_update/stamp/" ++ Id,
    Headers = [{"Content-Type","application/json"}|
               proplists:get_value(headers, S)],
    update(Url, Headers, Json);
update(design, Id, Json, R, S) ->
    update(design, Id, Json, utils:adb() ++ wrq:path_info(project, R), R, S).
update(design, Id, Json, DB, R, S) ->
    Json1 = jsn:decode(Json),
    Version = jsn:get_value(<<"version">>, Json1),
    case get_design_rev(Id, R, S) of
        {Version, _} -> {ok, updated};
        {error, Error} -> {error, Error};
        {_, Rev} ->
            Json2 = jsn:set_value(<<"_rev">>, Rev, Json1),
            Url = DB ++ "/_design/" ++ Id,
            Headers = [{"Content-Type","application/json"}],
            update(Url, Headers, jsn:encode(Json2))
    end.
  
update(design, Json, R, S) ->
    update(design, wrq:path_info(id, R), Json, R, S);

update(bulk, Json, R, S) ->
    Url = utils:ndb() ++ wrq:path_info(project, R) ++ "/_bulk_docs",
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
        {ok, "201", _, _} -> 
            {ok, updated};
        {error, req_timedout} -> 
            {error, req_timedout};
        {ok, "403", _, Body} ->
            Resp = jsn:decode(Body),
            Message = jsn:get_value(<<"reason">>, Resp),
            {403, Message};
        {ok, "409", _, _} -> 
            {409, <<"Conflict">>}
    end.

update_doctype_version(R, S) ->
    Doctype = wrq:path_info(doctype, R),
    {ok, Json} = get(Doctype, R, S),
    {ok, updated} = update(doc, Doctype, Json, R, S).

exists(Target, R, S) ->
    exists(Target, utils:ndb() ++ wrq:path_info(project, R), R, S).

exists(Target, DB, _R, S) ->
    Headers = proplists:get_value(headers, S),
    BaseUrl = DB ++ "/",
    case ibrowse:send_req(BaseUrl ++ Target, Headers, head) of
        {ok, "200", _, _} -> true;
        {ok, "404", _, _} -> false
    end.
