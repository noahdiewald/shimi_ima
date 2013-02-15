%%% Copyright 2012 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of Ʃimi Ima.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% dictionary_maker is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General
%%% Public License along with dictionary_maker. If not, see
%%% <http://www.gnu.org/licenses/>.

%%% @copyright 2012 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc The is the resource used for managing projects.

-module(project_resource).
-author('Noah Diewald <noah@diewald.me>').

-export([init/3]).
-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         create_path/2,
         delete_resource/2,
         from_json/2,
         index_html/2,
         is_authorized/2,
         main_html/2,
         post_is_create/2,
         rest_init/2
        ]).
-export([
         validate_authentication/3
        ]).

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

is_authorized(R, S) ->
    case cowboy_req:method(R) of
        {<<"DELETE">>, R1} ->
            proxy_auth:is_authorized(R1, [{source_mod, ?MODULE}|S]);
        {<<"POST">>, R1} ->
            proxy_auth:is_authorized(R1, [{source_mod, ?MODULE}|S]);
        {_, R1} ->
            {true, R1, S}
    end.

allowed_methods(R, S) ->
    case proplists:get_value(target, S) of
        index -> {[<<"HEAD">>, <<"GET">>, <<"POST">>], R, S};
        main -> {[<<"HEAD">>, <<"GET">>], R, S};
        identifier -> {[<<"HEAD">>, <<"DELETE">>], R, S}
    end.
  
content_types_accepted(R, S) ->
    h:accept_json(R, S).

content_types_provided(R, S) ->
    case proplists:get_value(target, S) of
        main -> {[{{<<"text">>, <<"html">>, []}, main_html}], R, S};
        index -> {[{{<<"text">>, <<"html">>, []}, index_html}], R, S};
        identifier -> {[{{<<"text">>, <<"html">>, []}, index_html}], R, S}
    end.
  
delete_resource(R, S) ->
    h:delete_project(R, S).
  
post_is_create(R, S) ->
    {true, R, S}.

create_path(R, S) ->
    Uuid = list_to_binary("project-" ++ utils:uuid()),
    {Uuid, R, [{newid, Uuid}|S]}.
  
index_html(R, S) ->
    Json = couch:get_dbs(),
    {renderings(Json), R, S}.
  
main_html(R, S) ->
    User = proplists:get_value(user, S),
    {ok, Html} = render:render(projects_dtl, [{title, "Projects"}, {user, User}]),
    {Html, R, S}.
  
from_json(R, S) ->
    {Body, R1} = cowboy_req:body(R),
    ProjectData = jsn:decode(Body),
    {ok, ProjectId} = project:create(ProjectData, S),
    database_seqs:set_seq(ProjectId, 0),
    {true, R1, S}.

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
    F = fun (X) ->
        jsn:get_value(<<"id">>, X) /= <<"_design/shimi_ima">>
    end,
    [render_row(Project) || Project <- lists:filter(F, Rows)].
  
render_row(Project) ->
    {ok, Rendering} = render:render(project_list_elements_dtl, Project),
    iolist_to_binary(Rendering).
