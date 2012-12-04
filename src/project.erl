%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of dictionary_maker.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
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
%%% @doc This module contains functions for manipulating projects

-module(project).

-export([
         create/3,
         upgrade/1
        ]).

-include_lib("types.hrl").

%% @doc Create a project
-spec create(string(), string(), [{atom, any()}]) -> ok.
create(ProjectId, ProjectData, S) ->
    DBName = "project-" ++ ProjectId,
    ProjectData1 = jsn:encode(jsn:set_value(<<"_id">>, list_to_binary(ProjectId), ProjectData)),
    {ok, newdb} = couch:new_db(DBName),
    {ok, created} = couch:create("shimi_ima", ProjectData1, S),
    {ok, replicated} = couch:replicate("shimi_ima", DBName),
    ok.
    
%% @doc Update a project's design documents.
-spec upgrade(string()) -> ok.
upgrade(Project) ->
    {ok, replicated} = couch:replicate("shimi_ima", Project),
    ok.
