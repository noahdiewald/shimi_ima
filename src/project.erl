%%% Copyright 2013 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of dictionary_maker.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% dictionary_maker is distributed in the hope that it will be
%%% useful, but WITHOUT ANY WARRANTY; without even the implied
%%% warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
%%% PURPOSE. See the GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with dictionary_maker. If not, see
%%% <http://www.gnu.org/licenses/>.

%%% @copyright 2011 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc This module contains functions for manipulating projects

-module(project).

-export([
         all/0,
         create/2,
         upgrade/1
        ]).

-include_lib("types.hrl").

%% @doc Return a listing of all projects.
-spec all() -> jsn:json_term().
all() ->
    Json = couch:get_dbs(),
    Rows = jsn:get_value(<<"rows">>, Json),
    jsn:set_value(<<"rows">>, extract_process(Rows, []), Json).
    
%% @doc Create a project
-spec create(string(), h:req_state()) -> ok.
create(ProjectData, S) ->
    Id = utils:uuid(),
    DBName = "project-" ++ Id,
    ProjectData1 = jsn:set_value(<<"_id">>, list_to_binary(Id), ProjectData),
    {ok, newdb} = couch:new_db(DBName),
    {ok, _} = couch:create(ProjectData1, "shimi_ima", S),
    {ok, replicated} = couch:replicate("shimi_ima", DBName, "upgrade"),
    {ok, DBName}.
    
%% @doc Update a project's design documents.
-spec upgrade(string()) -> ok.
upgrade(Project) ->
    {ok, replicated} = couch:replicate("shimi_ima", Project, "upgrade"),
    ok.

%% @doc Extract the project ids and append the rest of the database
%% identifier.
-spec extract_process([jsn:json_term()], []) -> [jsn:json_term()].
extract_process([], Acc) ->
    Acc;
extract_process([H|T], Acc) ->
    case jsn:get_value(<<"id">>, H) of
        <<"_design/shimi_ima">> -> 
            extract_process(T, Acc);
        Id ->
            H2 = jsn:set_value(<<"id">>, << "project-", Id/binary >>, H),
            extract_process(T, [H2|Acc])
    end.
