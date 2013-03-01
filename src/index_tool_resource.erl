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
%%% @doc This module contains functions used to present the user with
%%% tools for building indexes to organize documents.

-module(index_tool_resource).
-author('Noah Diewald <noah@diewald.me>').

-export([init/3]).
-export([
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2, 
         resource_exists/2,
         rest_init/2,
         to_html/2
        ]).
-export([
         validate_authentication/3
        ]).

-include_lib("types.hrl").

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

allowed_methods(R, S) ->
    {[<<"HEAD">>, <<"GET">>], R, S}.

content_types_provided(R, S) ->
    {[{{<<"text">>, <<"html">>, []}, to_html}], R, S}.

to_html(R, S) ->
    {{ok, ProjectData}, R1} = h:project_data(R, S),
    {QsVals, R2} = cowboy_req:qs_vals(R1),
    {Project, R3} = h:project(R2),
    {ok, Doctypes} = q:doctypes(QsVals, Project, S),
    Vals = [{<<"project_info">>, ProjectData},
        {<<"doctypes">>, Doctypes},
        {<<"user">>, proplists:get_value(user, S)}],
    {ok, Html} = render:render(index_tool_dtl, Vals),
    {Html, R3, S}.

resource_exists(R, S) ->
    {Exist, R2} = h:exists("", R, S),
    {Exist, R2, S}.

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

validate_authentication(Props, R, S) ->
    {{ok, ProjectData}, R1} = h:project_data(R, S),
    Name = jsn:get_value(<<"name">>, ProjectData),
    ValidRoles = [<<"_admin">>, <<"manager">>, Name],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R1, S};
        false -> {proplists:get_value(auth_head, S), R1, S}
    end.
