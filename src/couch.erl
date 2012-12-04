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
         create/3,
         delete/2,
         exists/3,
         exists/4,
         fold_view/6,
         get/3,
         get/4,
         get_db_seq/1,
         get_dbs/0,
         get_design_rev/3,
         get_view_json/4,
         get_view_json/5,
         get_view_json/6,
         get_views/1,
         get_uuid/2,
         new_db/1,
         replicate/2,
         should_wait/2,
         update/4,
         update/5,
         update/6,
         update_doctype_version/2
        ]).

-include_lib("types.hrl").

%% @doc Create a document
-spec create(string(), jsn:json_term(), [{atom, any()}]) -> {ok, created} | {integer(), binary()}.
create(Url=[$h,$t,$t,$p,$:|_], Json, S) ->
    Headers = [{"Content-Type","application/json"}|proplists:get_value(headers, S)],
    case ibrowse:send_req(Url, Headers, post, jsn:encode(jsn:decode(Json))) of
        {ok, "201", _, _} ->
            {ok, created};
        {ok, "403", _, Body} ->
            Resp = jsn:decode(Body),
            Message = jsn:get_value(<<"reason">>, Resp),
            {403, Message}
    end;
create(Project, Json, S) ->
    DB = case proplists:get_value(admin, S) of
        true ->
            utils:adb();
        _ ->
            utils:ndb()
    end,
    create(DB ++ Project ++ "/_design/shimi_ima/_update/stamp", Json, S).

%% @doc Get a document
-spec get(string(), string(), [{atom, any()}]) -> {ok, jsn:json_term()} | {integer(), binary()} | {error(), binary()}.
get(Id, Project, S) -> undefined.

%% @doc Get a specific revision of a document
-spec get(string(), string(), string(), [{atom, any()}]) -> {ok, jsn:json_term()} | {integer(), binary()} | {error(), binary()}.
get(Id, Rev, Project, S) -> undefined.

%% @doc Make a new database
new_db(DB) ->
    {ok, "201", _, _} = ibrowse:send_req(utils:adb() ++ DB, [], put),
    {ok, newdb}.

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

get(Id, Project, H) ->  
    Headers = proplists:get_value(headers, H),
    DataBaseUrl = utils:ndb() ++ Project ++ "/",
    get_helper(DataBaseUrl ++ Id, Headers).
  
get_json(project, Project, _H) ->
    Id = Project -- "project-",
    Url = utils:adb() ++ "projects/" ++ Id,
    get_json_helper(Url, []);
  
get_json(doctype, Project, S) ->
    Headers = proplists:get_value(headers, S),
    DataBaseUrl = utils:ndb() ++ Project ++ "/",
    Doctype = wrq:path_info(doctype, R),
    get_json_helper(DataBaseUrl ++ Doctype, Headers);
  
get_json(id, Project, H) ->
    Headers = proplists:get_value(headers, H),
    DataBaseUrl = utils:ndb() ++ Project ++ "/",
    Id = wrq:path_info(id, R),
    Url = DataBaseUrl ++ Id ++ "?revs_info=true",
    get_json_helper(Url, Headers);
  
get_json(rev, Project, H) ->
    Revision = case wrq:get_qs_value("rev", R) of
              undefined ->
                  wrq:path_info(rev, R);
              Rev -> Rev
          end,
    Headers = proplists:get_value(headers, H),
    Url = utils:ndb() ++ Project ++ "/" ++ 
        wrq:path_info(id, R) ++ "?rev=" ++ Revision,
    get_json_helper(Url, Headers);
  
get_json(Id, Project, H) ->
    Headers = proplists:get_value(headers, H),
    DataBaseUrl = utils:ndb() ++ Project ++ "/",
    get_json_helper(DataBaseUrl ++ Id, Headers).
    
get_json(safer, Id, Project, H) ->
    Headers = proplists:get_value(headers, H),
    DataBaseUrl = utils:ndb() ++ Project ++ "/",
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
    
get_view_json_helper(Id, Name, Qs, Project, H) ->
    Headers = proplists:get_value(headers, H),
    Url = utils:ndb() ++ Project ++ "/",
    Path = "_design/" ++ Id ++ "/_view/" ++ Name,
    FullUrl = Url ++ Path ++ Qs,
    case get_json_helper(safer, FullUrl, Headers) of
        undefined -> {error, not_found};
        {error, req_timedout} -> {error, req_timedout};
        {error, not_found} -> {error, not_found};
        {ok, Json} -> {ok, Json}
    end.

fold_view(Id, Name, Qs, Fun, Project, H) ->
    {ok, ViewJson} = get_view_json(Id, Name, Qs, Project, H),
    Rows = jsn:get_value(<<"rows">>, ViewJson),
    lists:foldr(Fun, [], Rows).

get_view_json(Id, Name, Project, H) ->
    Qs = view:normalize_vq(R),
    get_view_json(Id, Name, Qs, Project, H).

get_view_json(noqs, Id, Name, Project, H) ->
    get_view_json_helper(Id, Name, [], Project, H);
get_view_json(sortkeys, Id, Name, Project, H) ->
    Qs = view:normalize_sortkey_vq(Id, Project, H),
    get_view_json_helper(Id, Name, "?" ++ Qs, Project, H);
get_view_json(Id, Name, Qs, Project, H) ->
    get_view_json_helper(Id, Name, "?" ++ Qs, Project, H).

get_view_json(sortkeys, Id, Name, Doctype, Project, H) ->
    Qs = view:normalize_sortkey_vq(Doctype, Project, H),
    get_view_json_helper(Id, Name, "?" ++ Qs, Project, H).

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
    get_views(Project).

get_design_rev(Name, Project, H) ->
    Id = case Name of
             [$_|_] -> Name;
             _Else -> "_design/" ++ Name
         end,
    Url = case proplists:get_value(db, H) of
              undefined -> utils:adb() ++ Project ++ "/" ++ Id;
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
get_uuid(_R, _H) ->
    utils:uuid().

delete(R, H) ->
    Id = wrq:path_info(id, R),
    Rev = wrq:get_qs_value("rev", R),
    Url = utils:ndb() ++ Project ++ "/" ++ Id ++ "?rev=" ++ Rev,
    Headers = [{"Content-Type",
                "application/json"}|proplists:get_value(headers, H)],
    case ibrowse:send_req(Url, Headers, delete) of
        {ok, "200", _, _} -> 
            case exists("_design/" ++ Id, Project, H) of
                true ->
                    {_, DRev} = get_design_rev(Id, Project, H),
                    DUrl = utils:ndb() ++ Project ++ 
                        "/_design/" ++ Id ++ "?rev=" ++ DRev,
                    {ok, "200", _, _} = ibrowse:send_req(DUrl, Headers, delete);
                false -> ok
            end,
            {ok, deleted};
        {ok, "409", _, _} -> {409, <<"Conflict">>}
    end.

bulk_update(Docs, Project, H) ->
    Headers = [{"Content-Type","application/json"}|
               proplists:get_value(headers, H)],
    Url = utils:ndb() ++ Project ++ "/_bulk_docs",
    {ok, _, _, Body} = ibrowse:send_req(Url, Headers, post, jsn:encode(Docs)),
    jsn:decode(Body).
  
update(doc, Id, Json, Project, H) ->
    Project =  Project,
    Url = utils:ndb() ++ Project ++ "/_design/shimi_ima/_update/stamp/" ++ Id,
    Headers = [{"Content-Type","application/json"}|
               proplists:get_value(headers, H)],
    update(Url, Headers, Json);
update(design, Id, Json, Project, H) ->
    update(design, Id, Json, utils:adb() ++ Project, Project, H).
update(design, Id, Json, DB, Project, H) ->
    Json1 = jsn:decode(Json),
    Version = jsn:get_value(<<"version">>, Json1),
    case get_design_rev(Id, Project, H) of
        {Version, _} -> {ok, updated};
        {error, Error} -> {error, Error};
        {_, Rev} ->
            Json2 = jsn:set_value(<<"_rev">>, Rev, Json1),
            Url = DB ++ "/_design/" ++ Id,
            Headers = [{"Content-Type","application/json"}],
            update(Url, Headers, jsn:encode(Json2))
    end.
  
update(design, Json, Project, H) ->
    update(design, wrq:path_info(id, R), Json, Project, H);

update(bulk, Json, Project, H) ->
    Url = utils:ndb() ++ Project ++ "/_bulk_docs",
    Headers = [{"Content-Type","application/json"}|proplists:get_value(headers, H)],
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

update_doctype_version(R, H) ->
    Doctype = wrq:path_info(doctype, R),
    {ok, Json} = get(Doctype, Project, H),
    {ok, updated} = update(doc, Doctype, Json, Project, H).

exists(Target, Project, H) ->
    exists(Target, utils:ndb() ++ Project, Project, H).

exists(Target, DB, _R, H) ->
    BaseUrl = DB ++ "/",
    case ibrowse:send_req(BaseUrl ++ Target, H, head) of
        {ok, "200", _, _} -> true;
        {ok, "404", _, _} -> false
    end.
