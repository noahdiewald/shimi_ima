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
%%% @doc Index resource.

-module(changelog_resource).
-author('Noah Diewald <noah@diewald.me>').

-export([init/3]).
-export([
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         resource_exists/2,
         rest_init/2,
         to_json/2
        ]).
-export([
         validate_authentication/3
        ]).

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

resource_exists(R, S) ->
    {[Doctype, Project], R1} = h:g([doctype, project], R),
    S1 = [{doctype, Doctype}, {project, Project}|S],
    case proplists:get_value(target, S1) of
        identifier -> h:exists_id(R, S1);
        index -> h:exists_with_deps([Doctype], R1, S1)
    end. 

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    case proplists:get_value(target, S) of
        index -> {[<<"HEAD">>, <<"GET">>], R, S};
        identifier -> {[<<"HEAD">>, <<"GET">>], R, S}
    end.
  
content_types_provided(R, S) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], R, S}.
  
to_json(R, S) ->
    case proplists:get_value(target, S) of
        index -> json_index(R, S);
        identifier -> json_change(R, S)
    end.
  
% Helpers
  
json_index(R, S) ->
    Doctype = list_to_binary(proplists:get_value(doctype, S)),
    {QsVals, R1} = cowboy_req:qs_vals(R),
    StartKey = case proplists:get_value(<<"startkey">>, QsVals) of
                  undefined -> << Doctype/binary, "-a" >>;
                  <<>> -> << Doctype/binary, "-a" >>;
                  <<"\"\"">> -> << Doctype/binary, "-a" >>;
                  Key when is_binary(Key) ->
                      DatePart = re:replace(Key, <<"[^0-9]">>, <<>>, [global,unicode,{return, binary}]),
                      << Doctype/binary, "-", DatePart/binary >>
              end,
    QsVals2 = [{<<"startkey">>, StartKey},
               {<<"descending">>, true},
               {<<"endkey">>, << Doctype/binary, "-0" >>}|QsVals],
    {{ok, Json}, R2} = q:changelog(QsVals2, R1, S),
    {jsn:encode(Json), R2, S}.

json_change(R, S) ->
    {<<>>, R, S}.
    
validate_authentication(Props, R, S) ->
    {{ok, ProjectData}, R1} = h:project_data(R, S),
    Name = jsn:get_value(<<"name">>, ProjectData),
    ValidRoles = [<<"_admin">>, <<"manager">>, Name],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R1, S};
        false -> {proplists:get_value(auth_head, S), R1, S}
    end.
