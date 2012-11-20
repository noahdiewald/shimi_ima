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
%%% @doc This module contains functions used to present the user with
%%% tools for building indexes to organize documents.

-module(index_tool_resource).

-export([
         init/1,
         is_authorized/2, 
         resource_exists/2,
         to_html/2,
         validate_authentication/3
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("config.hrl").
-include_lib("types.hrl").

init(Opts) -> {ok, Opts}.

to_html(R, S) ->
    User = proplists:get_value(user, S),
    Project = couch:get_json(project, R, S),
    {ok, Doctypes} = q:doctypes(R, S),
  
    Vals = [{<<"user">>, User},
            {<<"project_info">>, Project},
            {<<"doctypes">>, Doctypes}],
  
    {ok, Html} = index_tool_dtl:render(Vals),
    {Html, R, S}.

resource_exists(R, S) ->
    DatabaseUrl = utils:adb() ++ wrq:path_info(project, R),
  
    case ibrowse:send_req(DatabaseUrl, [], head) of
        {ok, "200", _, _} -> {true, R, S};
        {ok, "404", _, _} -> {false, R, S}
    end. 

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

validate_authentication(Props, R, S) ->
    Project = couch:get_json(project, R, S),
    Name = jsn:get_value(<<"name">>, Project),
    ValidRoles = [<<"_admin">>, <<"manager">>, Name],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R, S};
        false -> {proplists:get_value(auth_head, S), R, S}
    end.
