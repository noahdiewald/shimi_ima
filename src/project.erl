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

-export([upgrade/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("config.hrl").
-include_lib("types.hrl").

%% @doc Update a project's design documents.

-spec upgrade(R :: utils:reqdata(), S :: any()) -> ok.
upgrade(R, S) ->
    [ok, ok, ok, ok, ok] = lists:map(fun(X) -> upgrade_design(X, R, S) end,
                                     ["doctypes", "indexes", "charseqs", 
                                      "fieldsets", "fields"]),
    upgrade_doctype_designs(R, S),
    %{ok, updated} = upgrade_file_manager(R, S),
    ok.

upgrade_doctype_designs(R, S) ->
    F = fun(X) ->
                Id = jsn:get_value(<<"id">>, X),
                {ok, Design} = 
                    design_doctype_json_dtl:render(
                      jsn:set_value(<<"_id">>, Id, X)),
                do_upgrade(Id, Design, R, S)
        end,

    case q:doctypes(R, S) of
        {ok, Json} ->
            lists:map(F, jsn:get_value(<<"rows">>, Json));
        {error, req_timedout} ->
            upgrade_doctype_designs(R, S)
    end.
    
upgrade_design(Id, R, S) ->
    Template = list_to_atom("design_" ++ Id ++ "_json_dtl"),
    {ok, Json} = Template:render(),
    do_upgrade(Id, Json, R, S).

do_upgrade(Id, Json, R, S) ->
    case couch:update(design, Id, Json, R, S) of
        {ok, updated} ->
            ok;
        {error, not_found} ->
            {ok, created} = couch:create(design, Json, R, S),
            ok
    end.

upgrade_file_manager(R, S) -> 
    {ok, Fmanager} = design_file_manager_json_dtl:render(),
    {R, S1} = attach:get_database(R, S),
    Id = binary_to_list(jsn:get_value(<<"_id">>, jsn:decode(Fmanager))),
    DB = proplists:get_value(db, S1),
    couch:update(design, Id, Fmanager, DB, R, S1).
