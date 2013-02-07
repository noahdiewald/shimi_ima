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
         exists/3,
         fold_view/6,
         get/3,
         get/4,
         get_db_seq/1,
         get_dbs/0,
         get_design_rev/3,
         get_view_json/3,
         get_view_json/5,
         get_views/1,
         new_db/1,
         replicate/2,
         replicate/3,
         rm_db/1,
         should_wait/2,
         update/4
        ]).

-include_lib("types.hrl").

adb(Project) ->
    {ok, Val} = application:get_env(dictionary_maker, admin_db),
    Val ++ Project ++ "/".

%% @doc Create a document
-spec create(jsn:json_term(), string(), [tuple()]) -> {ok, created} | {forbidden, binary()}.
create(Json, Url=[$h,$t,$t,$p,$:|_], Headers) ->
    case ibrowse:send_req(Url, Headers, post, jsn:encode(Json)) of
        {ok, "201", _, _} ->
            {ok, created};
        {ok, "403", _, Body} ->
            Resp = jsn:decode(Body),
            Message = jsn:get_value(<<"reason">>, Resp),
            {forbidden, Message}
    end;
create(Json, Project, S) ->
    CT = [{"Content-Type","application/json"}],
    {DB, Headers} = case proplists:get_value(admin, S) of
        true ->
            {adb(Project), CT};
        _ ->
            {ndb(Project), CT ++ proplists:get_value(headers, S, [])}
    end,
    create(Json, DB ++ "_design/shimi_ima/_update/stamp", Headers).

-spec delete(string(), string(), string(), h:req_state()) -> h:req_retval().
delete(Id, Rev, Project, S) ->
    Url = ndb(Project) ++ Id ++ "?rev=" ++ Rev,
    Headers = [{"Content-Type",
                "application/json"}|proplists:get_value(headers, S)],
    case ibrowse:send_req(Url, Headers, delete) of
        {ok, "200", _, _} ->
            % User Indexes have associated design docs
            case exists("_design/" ++ Id, Project, S) of
                true ->
                    {_, DRev} = get_design_rev(Id, Project, [{"Content-Type", "application/json"}]),
                    DUrl = ndb(Project) ++ "_design/" ++ Id ++ "?rev=" ++ DRev,
                    {ok, "200", _, _} = ibrowse:send_req(DUrl, Headers, delete);
                false -> ok
            end,
            {ok, deleted};
        {ok, "409", _, _} -> {error, conflict}
    end.

-spec exists(string(), string(), h:req_state()) -> boolean().
exists(Id, Project, S) ->
    Url = ndb(Project) ++ Id,
    Headers = proplists:get_value(headers, S, []),
    case ibrowse:send_req(Url, Headers, head) of
        {ok, "200", _, _} -> true;
        {ok, "404", _, _} -> false
    end.

-spec fold_view(string(), string(), string(), fun((jsn:json_term(), [jsn:json_term()], string()) -> any()), string(), h:req_state()) -> any().
fold_view(Id, Name, Qs, Fun, Project, S) ->
    {ok, ViewJson} = get_view_json(Id, Name, Qs, Project, S),
    Rows = jsn:get_value(<<"rows">>, ViewJson),
    lists:foldr(Fun, [], Rows).

%% @doc Get a document
-spec get(string(), string(), h:req_state()) -> {ok, jsn:json_term()} | {error, atom()}.
get(Id, Project, S) ->  
    Headers = proplists:get_value(headers, S, []),
    Url = case proplists:get_value(revs_info, S) of
        undefined ->
            ndb(Project) ++ Id;
        true ->
            ndb(Project) ++ Id ++ "?revs_info=true"
    end,
    get_json_helper(Url, Headers).
    
%% @doc Get a specific revision of a document
-spec get(string(), string(), string(), h:req_state()) -> {ok, jsn:json_term()} | {error, atom()}.
get(Id, Rev, Project, S) ->
    Headers = proplists:get_value(headers, S, []),
    Url = ndb(Project) ++ Id ++ "?rev=" ++ Rev,
    get_json_helper(Url, Headers).

-spec get_db_info(string()) -> {ok, jsn:json_term()} | {error, atom()}.
get_db_info(Project) ->
    get_json_helper(adb(Project), []).

-spec get_db_seq(string()) -> {ok, binary()} | {error, atom()}.
get_db_seq(Project) ->
    case get_db_info(Project) of
        {ok, Json} -> {ok, jsn:get_value(<<"update_seq">>, Json)};
        Otherwise -> Otherwise
    end.

-spec get_dbs() -> jsn:json_term().
get_dbs() ->
    Url = pdb() ++ "_all_docs?include_docs=true",
    {ok, Json} = get_json_helper(Url, []),
    Json.

-spec get_design_rev(string(), string(), h:req_state()) -> {ok, jsn:json_term()} | {error, atom()}.
get_design_rev(Name, Project, Headers) ->
    Url = adb(Project) ++ "_design/" ++ Name,
    case get_json_helper(Url, Headers) of
        {ok, Json} ->
            {jsn:get_value(<<"version">>, Json), 
             jsn:get_value(<<"_rev">>, Json)};
        Else -> Else
    end.

-spec get_helper(string(), [tuple()]) -> {ok, iolist()} | {error, atom()}.
get_helper(Url, Headers) ->
    Opts = [{connect_timeout, 500}, {inactivity_timeout, 10000}],
    case ibrowse:send_req(Url, Headers, get, [], Opts) of
        {ok, "200", _, Json} -> {ok, Json};
        {ok, "404", _, _} -> {error, not_found};
        {error, req_timedout} -> {error, req_timedout}
    end.

-spec get_json_helper(string(), [tuple()]) -> {ok, jsn:json_term()} | {error, atom()}.
get_json_helper(Url, Headers) ->  
    case get_helper(Url, Headers) of
        {ok, Json} -> {ok, jsn:decode(Json)};
        Else -> Else
    end.

-spec get_views(string()) -> [binary()].
get_views(Project) ->
    Qs = view:to_string(view:from_list([{"startkey", <<"_design/">>},
                                        {"endkey", <<"_design0">>},
                                        {"include_docs", true}])),
    Url = adb(Project) ++ "_all_docs" ++ "?" ++ Qs,
    {ok, Json} = get_json_helper(Url, []),
    Designs = proplists:get_value(<<"rows">>, Json),
    Filter = fun(X) -> 
                     case X of
                         undefined -> false;
                         _ -> true 
                     end
             end,
    lists:flatten(lists:filter(Filter, lists:map(fun get_view_path/1, Designs))).
    
-spec get_view_json(string(), string(), h:req_state()) -> {ok, jsn:json_term()} | {error, atom()}.
get_view_json(Qs, Project, S) ->
    Headers = proplists:get_value(headers, S, []),
    Url = ndb(Project),
    FullUrl = Url ++ "_all_docs" ++ "?" ++ Qs,
    case get_json_helper(FullUrl, Headers) of
        {error, req_timedout} -> {error, req_timedout};
        {error, not_found} -> {error, not_found};
        {ok, Json} -> {ok, Json}
    end.
    
-spec get_view_json(string(), string(), string(), string(), h:req_state()) -> {ok, jsn:json_term()} | {error, atom()}.
get_view_json(Id, Name, Qs, Project, S) ->
    Headers = proplists:get_value(headers, S, []),
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
-spec new_db(string()) -> {ok, newdb}.
new_db(DB) ->
    {ok, "201", _, _} = ibrowse:send_req(adb(DB), [], put),
    {ok, newdb}.

-spec ndb(string()) -> string().
ndb(Project) ->
    {ok, Val} = application:get_env(dictionary_maker, normal_db),
    Val ++ Project ++ "/".

-spec pdb() -> string().
pdb() ->
    {ok, Val} = application:get_env(dictionary_maker, admin_db),
    Val ++ "shimi_ima" ++ "/".

-spec replicate(string(), string()) -> {ok, replicated}.
replicate(Source, Target) ->
    Json = [{<<"source">>, list_to_binary(Source)}, {<<"target">>, list_to_binary(Target)}],
    replicate_helper(Json).

%% @doc Assumes that the source database and source design document have
%% the same name.
-spec replicate(string(), string(), string()) -> {ok, replicated}.
replicate(Source, Target, Filter) ->
    Json = [{<<"source">>, list_to_binary(Source)}, {<<"target">>, list_to_binary(Target)}, {<<"filter">>, list_to_binary(Source ++ "/" ++ Filter)}],
    replicate_helper(Json).
    
-spec replicate_helper(jsn:json_term()) -> {ok, replicated}.
replicate_helper(Json) ->
    Headers = [{"Content-Type", "application/json"}],
    Url = adb("_replicate"),
    {ok, [$2|_], _, _} = ibrowse:send_req(Url, Headers, post, jsn:encode(Json)),
    {ok, replicated}.

-spec rm_db(string()) -> h:req_retval().
rm_db(Db) ->
    Url = adb(Db),
    Headers = [{"Content-Type","application/json"}],
    case ibrowse:send_req(Url, Headers, delete) of
        {ok, "200", _, _} ->
            {ok, deleted};
        {ok, "409", _, _} ->
            {error, conflict}
    end.

-spec should_wait(string(), string()) -> boolean().
should_wait(Project, ViewPath) ->
    Url = adb(Project) ++ ViewPath ++ "?limit=1",
    case get_helper(Url, []) of
        {error, req_timedout} -> true;
        _ -> false
    end.
  
-spec update(string(), jsn:json_term(), string(), h:req_state()) -> {ok, updated} | {error, atom()} | {forbidden, binary()}.
update(Id, Json, Project, S) ->
    CT = [{"Content-Type","application/json"}],
    {DB, Headers} = case proplists:get_value(admin, S) of
        true ->
            {adb(Project), CT};
        _ ->
            {ndb(Project), CT ++ proplists:get_value(headers, S, [])}
    end,
    update(Json, DB ++ "_design/shimi_ima/_update/stamp/" ++ Id, Headers).

-spec update(string(), [tuple()], jsn:json_term()) -> {ok, updated} | {error, atom()} | {forbidden, binary()}.
update(Json, Url, Headers) ->
    case ibrowse:send_req(Url, Headers, put, jsn:encode(Json)) of
        {ok, "201", _, _} -> 
            {ok, updated};
        {error, req_timedout} -> 
            {error, req_timedout};
        {ok, "403", _, Body} ->
            Resp = jsn:decode(Body),
            Message = jsn:get_value(<<"reason">>, Resp),
            {forbidden, Message};
        {ok, "409", _, _} -> 
            {error, conflict}
    end.
