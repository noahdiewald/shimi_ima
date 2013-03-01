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
%%% @doc Currently focused on simply rendering the configuration page.

-module(config_resource).
-author('Noah Diewald <noah@diewald.me>').

-export([init/3]).
-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         is_authorized/2, 
         post_is_create/2,
         process_post/2,
         resource_exists/2,
         to_html/2,
         rest_init/2
        ]).
-export([
         validate_authentication/3
        ]).

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

resource_exists(R, S) ->
    {Project, R1} = cowboy_req:binding(project, R),
    DatabaseUrl = utils:adb() ++ binary_to_list(Project),
    case ibrowse:send_req(DatabaseUrl, [], head) of
        {ok, "200", _, _} -> {true, R1, S};
        {ok, "404", _, _} -> {false, R1, S}
    end. 

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    case proplists:get_value(target, S) of
        main -> {[<<"HEAD">>, <<"GET">>], R, S};
        upgrade -> {[<<"HEAD">>, <<"POST">>], R, S}
    end.
  
post_is_create(R, S) ->
    case proplists:get_value(target, S) of
        upgrade -> {false, R, S};
        _Else -> {true, R, S}
    end.

content_types_provided(R, S) ->
    case proplists:get_value(target, S) of
        main -> {[{{<<"text">>, <<"html">>, []}, to_html}], R, S};
        upgrade -> {[{{<<"*">>, <<"*">>, []}, to_html}], R, S}
    end.
  
content_types_accepted(R, S) ->
    h:accept_json(R, S).

process_post(R, _S) ->
    {Project, R1} = cowboy_req:binding(project, R),
    DatabaseUrl = utils:adb() ++ binary_to_list(Project),
    spawn_link(project, upgrade, [DatabaseUrl]),
    cowboy_req:reply(204, [], <<>>, R1).

to_html(R, S) ->
    User = proplists:get_value(user, S),
    {{ok, ProjectData}, R1} = h:project_data(R, S),
    Vals = [{<<"user">>, User},{<<"project_info">>, ProjectData}],
    {ok, Html} = render:render(config_dtl, Vals),
    {Html, R1, S}.

validate_authentication(Props, R, S) ->
    {{ok, ProjectData}, R1} = h:project_data(R, S),
    Name = jsn:get_value(<<"name">>, ProjectData),
    ValidRoles = [<<"_admin">>, <<"manager">>, Name],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R1, S};
        false -> {proplists:get_value(auth_head, S), R1, S}
    end.
