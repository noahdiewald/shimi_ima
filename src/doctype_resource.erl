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
%%% @doc Doctype resource 

-module(doctype_resource).
-author('Noah Diewald <noah@diewald.me>').

-export([init/3]).
-export([
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         rest_init/2,
         to_html/2,
         to_json/2
        ]).
-export([
         validate_authentication/3
        ]).

-include_lib("types.hrl").

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    {[<<"HEAD">>, <<"GET">>], R, S}.

content_types_provided(R, S) ->
    case proplists:get_value(target, S) of
        index -> {[{{<<"text">>, <<"html">>, []}, to_html}], R, S};
        identifier -> {[{{<<"*">>, <<"*">>, []}, to_json}], R, S}
    end.

to_html(R, S) ->
    User = proplists:get_value(user, S),
    {{ok, ProjectData}, R1} = h:project_data(R, S),
    {QsVals, R2} = cowboy_req:qs_values(R1),
    {Project, R3} = h:project(R2),
    {ok, Json} = q:doctypes(QsVals, Project, S),
    Vals = [{<<"title">>, <<"All Document Types">>},
            {<<"project_info">>, ProjectData},
            {<<"user">>, User},
            {<<"doctypes">>, jsn:get_value(<<"rows">>, Json)}],
    {ok, Html} = render:render(doctype_index_dtl, Vals),
    {Html, R3, S}.
  
to_json(R, S) ->
    {[Project, Doctype], R1} = h:g([project, doctype], R),
    S1 = [{project, Project}, {doctype, Doctype}|S],
    {{ok, DocData}, R2} = h:doctype_data(R1, S1),
    {jsn:encode(document:normalize(DocData, S)), R2, S1}.
  
% Helpers

validate_authentication(Props, R, S) ->
    {{ok, ProjectData}, R1} = h:project_data(R, S),
    Name = jsn:get_value(<<"name">>, ProjectData),
    NormalRoles = [<<"_admin">>, <<"manager">>, Name],
    IsNormal = fun (Role) -> lists:member(Role, NormalRoles) end,
    IsAdmin = fun (Role) -> lists:member(Role, [<<"_admin">>]) end,
    Normal = lists:any(IsNormal, proplists:get_value(<<"roles">>, Props)),
    Admin = lists:any(IsAdmin, proplists:get_value(<<"roles">>, Props)),
    Target = proplists:get_value(target, S),
  
    case {Target, Normal, Admin} of
        {index, true, _} -> {true, R1, S};
        {index, false, _} -> {proplists:get_value(auth_head, S), R1, S};
        {identifier, true, _} -> {true, R1, S};
        {identifier, false, _} -> {proplists:get_value(auth_head, S), R1, S};
        {touch, _, true} -> {true, R1, S};
        {touch, _, false} -> {proplists:get_value(auth_head, S), R1, S}
    end.
