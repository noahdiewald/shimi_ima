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
%%% @doc The is the resource used for managing projects.

-module(project_resource).

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
         index_html/2,
         main_html/2
        ]).

% Custom
-export([
         validate_authentication/3
        ]).

-include_lib("webmachine/include/webmachine.hrl").

% Standard webmachine functions

init(Opts) -> {ok, Opts}.

resource_exists(R, S) ->
    Headers = proplists:get_value(headers, S),
    DatabaseUrl = utils:ndb() ++ "projects/",
  
    Id = wrq:path_info(id, R),
  
    Resp = case proplists:get_value(target, S) of
               identifier -> ibrowse:send_req(DatabaseUrl ++ Id, Headers, head);
               _ -> 
                   % Create the database if it doesn't exist
                   case ibrowse:send_req(DatabaseUrl, Headers, head) of
                       {ok, "404", _, _} ->
                           create_database(),
                           ibrowse:send_req(DatabaseUrl, Headers, head);
                       Otherwise -> Otherwise
                   end
           end,
   
    case Resp of
        {ok, "200", _, _} -> {true, R, S};
        {ok, "404", _, _} -> {false, R, S}
    end. 

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    case proplists:get_value(target, S) of
        index -> {['HEAD', 'GET', 'POST'], R, S};
        main -> {['HEAD', 'GET'], R, S};
        identifier -> {['HEAD', 'DELETE'], R, S}
    end.
  
delete_resource(R, S) ->
    Headers = proplists:get_value(headers, S),
    ProjUrl = utils:ndb() ++ "projects/" ++ wrq:path_info(id, R),
  
    {ok, "200", _, Body} = ibrowse:send_req(ProjUrl, Headers, get),
    JsonIn = jsn:decode(Body),
  
    RevQs = "?rev=" ++ binary_to_list(jsn:get_value(<<"_rev">>, JsonIn)),
    ProjUrl1 = ProjUrl ++ RevQs,
    DatabaseUrl = utils:adb() ++ "project-" ++ wrq:path_info(id, R),
    
    case ibrowse:send_req(ProjUrl1, Headers, delete) of
        {ok, "200", _, _} -> 
            case ibrowse:send_req(DatabaseUrl, [], delete) of
                {ok, "200", _, _} -> {true, R, S}
            end
    end.
  
post_is_create(R, S) ->
    {true, R, S}.

create_path(R, S) ->
    Uuid = utils:uuid(),
    Location = "http://" ++ wrq:get_req_header("host", R) ++ "/" 
        ++ wrq:path(R) ++ "/" ++ Uuid,
    R1 = wrq:set_resp_header("Location", Location, R),
    {Uuid, R1, [{newid, Uuid}|S]}.

content_types_provided(R, S) ->
    case proplists:get_value(target, S) of
        main -> {[{"text/html", main_html}], R, S};
        index -> {[{"text/html", index_html}], R, S};
        identifier -> {[{"text/html", index_html}], R, S}
    end.
  
content_types_accepted(R, S) ->
    {[{"application/json", from_json}], R, S}.
  
index_html(R, S) ->
    Json = couch:get_dbs(),
    {renderings(Json), R, S}.
  
main_html(R, S) ->
    User = proplists:get_value(user, S),
    {ok, Html} = render:render(projects_dtl, [{title, "Projects"}, {user, User}]),
    {Html, R, S}.
  
from_json(R, S) ->
    DBName = "project-" ++ proplists:get_value(newid, S),
    NewDb = utils:adb() ++ DBName,
    ProjectsDb = utils:ndb() ++ "projects",
  
    JsonIn = jsn:decode(wrq:req_body(R)),
    JsonIn1 = jsn:set_value(<<"_id">>, 
                            list_to_binary(proplists:get_value(newid, S)), 
                            JsonIn),
    JsonOut = jsn:encode(JsonIn1),
  
    {ok, newdb} = couch:new_db(NewDb, R, S),
    {ok, created} = couch:create(direct, JsonOut, ProjectsDb, R, S),
    {ok, DoctypesDesign} = render:render(design_doctypes_json_dtl, []),
    {ok, created} = couch:create(design, DoctypesDesign, NewDb, R, S),
    {ok, CharseqsDesign} = render:render(design_charseqs_json_dtl, []),
    {ok, created} = couch:create(design, CharseqsDesign, NewDb, R, S),
    {ok, FieldsDesign} = render:render(design_fields_json_dtl, []),
    {ok, created} = couch:create(design, FieldsDesign, NewDb, R, S),
    {ok, FieldsetsDesign} = render:render(design_fieldsets_json_dtl, []),
    {ok, created} = couch:create(design, FieldsetsDesign, NewDb, R, S),
    {ok, FileManagerDesign} = render:render(design_file_manager_json_dtl, []),
    {ok, created} = couch:create(design, FileManagerDesign, NewDb, R, S),
    {ok, IndexesDesign} = render:render(design_indexes_json_dtl, []),
    {ok, created} = couch:create(design, IndexesDesign, NewDb, R, S),
    database_seqs:set_seq(DBName, 0),
    {true, R, S}.

% Helpers

validate_authentication(Props, R, S) ->
    ValidRoles = [<<"_admin">>, <<"manager">>],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R, S};
        false -> {proplists:get_value(auth_head, S), R, S}
    end.

% TODO: This is stupid. Fix it.
renderings(Json) ->
    Rows = jsn:get_value(<<"rows">>, Json),
    [render_row(Project) || Project <- Rows].
  
render_row(Project) ->
    {ok, Rendering} = render:render(project_list_elements_dtl, Project),
    iolist_to_binary(Rendering).

create_database() ->
    ContentType = {"Content-Type","application/json"},
    {ok, "201", _, _} = ibrowse:send_req(utils:adb() ++ "projects", [], put),
    {ok, ProjectDesign} = render:render(design_project_json_dtl, []),
    {ok, "201", _, _} = ibrowse:send_req(utils:adb() ++ "projects", [ContentType], 
                                         post, iolist_to_binary(ProjectDesign)).
