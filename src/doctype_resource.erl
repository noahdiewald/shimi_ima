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
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc Dictionary Maker main resource 

-module(doctype_resource).

% Webmachine API
-export([
  allowed_methods/2,
  content_types_provided/2,
  init/1, 
  is_authorized/2,
  to_html/2,
  to_json/2
]).

% Custom
-export([
  validate_authentication/3
]).

-include_lib("webmachine/include/webmachine.hrl").

% Standard webmachine functions

init(Opts) -> {ok, Opts}.

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    {['HEAD', 'GET'], R, S}.

content_types_provided(R, S) ->
    case proplists:get_value(target, S) of
        index -> {[{"text/html", to_html}], R, S};
        identifier -> {[{"*/*", to_json}], R, S}
    end.

to_html(R, S) ->
    {html_index(R, S), R, S}.
  
to_json(R, S) ->
    S1 = [{project, h:project(R)}, {doctype, h:doctype(R)}|S],
    {json_doctype(R, S1), R, S1}.
  
% Helpers

json_doctype(R, S) ->
    {ok, DocData} = h:doctype_data(R, S),
    jsn:encode(document:normalize(DocData, S)).

html_index(R, S) ->
    User = proplists:get_value(user, S),
    {ok, ProjectData} = h:project_data(R, S),
    {ok, Json} = q:doctypes(R, S),
    Vals = [{<<"title">>, <<"All Document Types">>},
            {<<"project_info">>, ProjectData},
            {<<"user">>, User},
            {<<"doctypes">>, jsn:get_value(<<"rows">>, Json)}],
    {ok, Html} = render:render(doctype_index_dtl, Vals),
    Html.

validate_authentication(Props, R, S) ->
    {ok, ProjectData} = h:project_data(R, S),
    Name = jsn:get_value(<<"name">>, ProjectData),
    NormalRoles = [<<"_admin">>, <<"manager">>, Name],
    IsNormal = fun (Role) -> lists:member(Role, NormalRoles) end,
    IsAdmin = fun (Role) -> lists:member(Role, [<<"_admin">>]) end,
    Normal = lists:any(IsNormal, proplists:get_value(<<"roles">>, Props)),
    Admin = lists:any(IsAdmin, proplists:get_value(<<"roles">>, Props)),
    Target = proplists:get_value(target, S),
  
    case {Target, Normal, Admin} of
        {index, true, _} -> {true, R, S};
        {index, false, _} -> {proplists:get_value(auth_head, S), R, S};
        {identifier, true, _} -> {true, R, S};
        {identifier, false, _} -> {proplists:get_value(auth_head, S), R, S};
        {touch, _, true} -> {true, R, S};
        {touch, _, false} -> {proplists:get_value(auth_head, S), R, S}
    end.
