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
         delete_resource/2,
         from_json/2,
         is_authorized/2,
         main_html/2,
         resource_exists/2,
         rest_init/2,
         to_json/2
        ]).
-export([
         validate_authentication/3
        ]).

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    case proplists:get_value(target, S) of
        index -> {[<<"HEAD">>, <<"GET">>, <<"POST">>], R, S};
        main -> {[<<"HEAD">>, <<"GET">>], R, S};
        identifier -> {[<<"HEAD">>, <<"DELETE">>], R, S}
    end.

resource_exists(R, S) ->
    h:exists_unless_post(R, S).

content_types_accepted(R, S) ->
    h:accept_json(R, S).

content_types_provided(R, S) ->
    case proplists:get_value(target, S) of
        main -> {[{{<<"text">>, <<"html">>, []}, main_html}], R, S};
        index -> {[{{<<"application">>, <<"json">>, []}, to_json}], R, S};
        identifier -> {[{{<<"application">>, <<"json">>, []}, to_json}], R, S}
    end.
  
delete_resource(R, S) ->
    h:delete_project(R, S).

main_html(R, S) ->
    User = proplists:get_value(user, S),
    {ok, Html} = render:render(projects_dtl, [{title, "Projects"}, {user, User}]),
    {Html, R, S}.

to_json(R, S) ->  
    {jsn:encode(project:all()), R, S}.
  
from_json(R, S) ->
    {ok, Body, R1} = cowboy_req:body(R),
    ProjectData = jsn:decode(Body),
    {ok, ProjectId} = project:create(ProjectData, S),
    database_seqs:set_seq(ProjectId, 0),
    {{true, list_to_binary([$/|ProjectId])}, R1, S}.

% Helpers

validate_authentication(Props, R, S) ->
    ValidRoles = [<<"_admin">>, <<"manager">>],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R, S};
        false -> {proplists:get_value(auth_head, S), R, S}
    end.
