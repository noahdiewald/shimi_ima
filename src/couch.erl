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
         create/3,
         delete/4,
         fold_view/6,
         get/3,
         get/4,
         get_db_seq/1,
         get_dbs/0,
         get_design_rev/3,
         get_view_json/5,
         get_views/1,
         new_db/1,
         replicate/2,
         should_wait/2,
         update/5
        ]).

-include_lib("types.hrl").

adb(Project) ->
    {ok, Val} = application:get_env(admin_db),
    Val ++ Project ++ "/".

%% @doc Create a document
-spec create(jsn:json_term(), string(), h:req_state()) -> h:req_retval().
create(Json, Url=[$h,$t,$t,$p,$:|_], S) ->
    Headers = [{"Content-Type","application/json"}|proplists:get_value(headers, S)],
    case ibrowse:send_req(Url, Headers, post, jsn:encode(jsn:decode(Json))) of
        {ok, "201", _, _} ->
            {ok, created};
        {ok, "403", _, Body} ->
            Resp = jsn:decode(Body),
            Message = jsn:get_value(<<"reason">>, Resp),
            {403, Message}
    end;
create(Json, Project, S) ->
    DB = case proplists:get_value(admin, S) of
        true ->
            adb(Project);
        _ ->
            ndb(Project)
    end,
    create(Json, DB ++ "_design/shimi_ima/_update/stamp", S).

-spec delete(string(), string(), h:req_state()) -> h:req_retval().
delete(Id, Rev, Project, S) ->
    Url = utils:ndb(Project) ++ Id ++ "?rev=" ++ Rev,
    Headers = [{"Content-Type",
                "application/json"}|proplists:get_value(headers, S)],
    case ibrowse:send_req(Url, Headers, delete) of
        {ok, "200", _, _} ->
            % User Indexes have associated design docs
            case exists("_design/" ++ Id, Project, S) of
                true ->
                    {_, DRev} = get_design_rev(Id, Project, S),
                    DUrl = ndb(Project) ++ "_design/" ++ Id ++ "?rev=" ++ DRev,
                    {ok, "200", _, _} = ibrowse:send_req(DUrl, Headers, delete);
                false -> ok
            end,
            {ok, deleted};
        {ok, "409", _, _} -> {error, conflict}
    end.

-spec exists(string(), string(), req_state()) -> boolean().
exists(Id, Project, S) ->
    Url = adb(Project) ++ Id,
    Headers = proplists:get_value(headers, H),
    case ibrowse:send_req(Url, Headers, head) of
        {ok, "200", _} -> true;
        {ok, "404", _} -> false
    end.

fold_view(Id, Name, Qs, Fun, Project, S) ->
    {ok, ViewJson} = get_view_json(Id, Name, Qs, Project, S),
    Rows = jsn:get_value(<<"rows">>, ViewJson),
    lists:foldr(Fun, [], Rows).

%% @doc Get a document
-spec get(string(), string(), [{atom, any()}]) -> {ok, jsn:json_term()} | {error(), atom()}.
get(Id, Project, S) ->  
    Headers = proplists:get_value(headers, S),
    Url = ndb(Project) ++ Id,
    get_json_helper(Url, Headers).
    
%% @doc Get a specific revision of a document
-spec get(string(), string(), string(), [{atom, any()}]) -> {ok, jsn:json_term()} | {error(), atom()}.
get(Id, Rev, Project, S) ->
    Headers = proplists:get_value(headers, S),
    Url = ndb(Project) ++ Id ++ "?rev=" ++ Rev,
    get_json_helper(Url, Headers).

get_db_info(Project) ->
    get_json_helper(adb(Project), []).

get_db_seq(Project) ->
    case get_db_info(Project) of
        {ok, Json} -> {ok, jsn:get_value(<<"update_seq">>, Json)};
        Otherwise -> Otherwise
    end.

get_dbs() ->
    Url = pdb() ++ "_all_docs?include_docs=true",
    {ok, Json} = get_json_helper(Url, []),
    Json.

get_design_rev(Name, Project, S) ->
    Url = utils:adb(Project) ++ "_design/" ++ Name,
    case get_json_helper(Url, []) of
        {ok, Json} ->
            {jsn:get_value(<<"version">>, Json), 
             jsn:get_value(<<"_rev">>, Json)};
        Else -> Else
    end.

-spec get_helper(string(), proplist()) -> {ok, iolist()} | {error, atom()}.
get_helper(Url, Headers) ->
    Opts = [{connect_timeout, 500}, {inactivity_timeout, 10000}],
    case ibrowse:send_req(Url, Headers, get, [], Opts) of
        {ok, "200", _, Json} -> {ok, Json};
        {ok, "404", _, _} -> {error, not_found};
        {error, req_timedout} -> {error, req_timedout}
    end.

-spec get_json_helper(string(), proplist()) -> {ok, jsn:json_term()} | {error, atom()}.
get_json_helper(Url, Headers) ->  
    case get_helper(Url, Headers) of
        {ok, Json} -> {ok, jsn:decode(Json)};
        Else -> Else
    end.

% TODO: DELETE
%get_view_json(Id, Name, Project, S) ->
%    Qs = view:normalize_vq(R),
%    get_view_json(Id, Name, Qs, Project, S).
% 
% get_view_json(noqs, Id, Name, Project, H) ->
%     get_view_json_helper(Id, Name, [], Project, H);
% get_view_json(sortkeys, Id, Name, Project, H) ->
%     Qs = view:normalize_sortkey_vq(Id, Project, H),
%     get_view_json_helper(Id, Name, "?" ++ Qs, Project, H);
% 
% get_view_json(sortkeys, Id, Name, Doctype, Project, H) ->
%     Qs = view:normalize_sortkey_vq(Doctype, Project, H),
%     get_view_json_helper(Id, Name, "?" ++ Qs, Project, H).

get_views(Project) ->
    Qs = view:to_string(view:from_list([{"startkey", <<"_design/">>},
                                        {"endkey", <<"_design0">>},
                                        {"include_docs", true}])),
    Url = utils:adb(Project) ++ "_all_docs" ++ "?" ++ Qs,
    {ok, Json} = get_json_helper(Url, []),
    Designs = proplists:get_value(<<"rows">>, Json),
    Filter = fun(X) -> 
                     case X of
                         undefined -> false;
                         _ -> true 
                     end
             end,
    lists:flatten(
      lists:filter(Filter, lists:map(fun get_view_path/1, Designs))).
    
-spec get_json_helper(string(), string(), sting(), string(), proplist()) -> {ok, jsn:json_term()} | {error, atom()}.
get_view_json(Id, Name, Qs, Project, S) ->
    Headers = proplists:get_value(headers, S),
    Url = ndb(Project),
    Path = "_design/" ++ Id ++ "/_view/" ++ Name,
    FullUrl = Url ++ Path ++ "?" ++ Qs,
    case get_json_helper(FullUrl, Headers) of
        {error, req_timedout} -> {error, req_timedout};
        {error, not_found} -> {error, not_found};
        {ok, Json} -> {ok, Json}
    end.

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

%% @doc Make a new database
new_db(DB) ->
    {ok, "201", _, _} = ibrowse:send_req(adb(DB), [], put),
    {ok, newdb}.

ndb(Project) ->
    {ok, Val} = application:get_env(normal_db),
    Val ++ Project ++ "/".

pdb() ->
    {ok, Val} = application:get_env(admin_db),
    Val ++ "shimi_ima" ++ "/".

replicate(Source, Target) ->
    Headers = [{"Content-Type", "application/json"}],
    Url = adb("_replicate"),
    Json = [{<<"source">>, list_to_binary(Source)},
            {<<"target">>, list_to_binary(Target)}],
    case ibrowse:send_req(Url, Headers, post, jsn:encode(Json)) of
        {ok, [$2|_], _, _} ->
            {ok, replicated};
        Otherwise ->
            Otherwise
    end.
    
-spec get_json_helper(string(), string()) -> boolean().
should_wait(Project, ViewPath) ->
    Url = adb(Project) ++ ViewPath ++ "?limit=1",
    case get_helper(Url, Headers) of
        {error, req_timedout} -> true;
        _ -> false
    end.
  
update(Id, Rev, Json, Project, S) ->
    Project =  Project,
    Url = ndb(Project) ++ "_design/shimi_ima/_update/stamp/" ++ Id,
    Headers = [{"Content-Type","application/json"}|
               proplists:get_value(headers, S)],
    update(Url, Headers, Json).
  
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
