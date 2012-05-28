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
%%% @doc This module contains functions for manipulating projects

-module(project).

-export([upgrade/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("config.hrl").
-include_lib("types.hrl").

%% @doc Update a project's design documents.

-spec upgrade(R :: utils:reqdata(), S :: any()) -> ok.
upgrade(R, S) ->
    {ok, updated} = upgrade_doctypes(R, S),
    {ok, updated} = upgrade_charseqs(R, S),
    {ok, updated} = upgrade_indexes(R, S),
    {ok, updated} = upgrade_file_manager(R, S),
    upgrade_all_doctypes(R, S),
    ok.

upgrade_indexes(R, S) ->
    {ok, Indexes} = design_indexes_json_dtl:render(),
    couch:update(design, "indexes", Indexes, R, S).

upgrade_doctypes(R, S) ->
    {ok, Dtypes} = design_doctypes_json_dtl:render(),
    couch:update(design, "doctypes", Dtypes, R, S).

upgrade_charseqs(R, S) ->
    {ok, Csets} = design_charseqs_json_dtl:render(),
    couch:update(design, "charseqs", Csets, R, S).

upgrade_file_manager(R, S) -> 
    {ok, Fmanager} = design_file_manager_json_dtl:render(),
    {R, S1} = attach:get_database(R, S),
    Id = binary_to_list(jsn:get_value(<<"_id">>, jsn:decode(Fmanager))),
    DB = proplists:get_value(db, S1),
    couch:update(design, Id, Fmanager, DB, R, S1).

upgrade_all_doctypes(R, S) ->
    {ok, VJson} = couch:get_view_json("doctypes", "all", R, S),
    Doctypes = [jsn:get_value(<<"value">>, X) || 
                   X <- jsn:get_value(<<"rows">>, VJson)],
    lists:map(fun (Doctype) -> upgrade_doctype(Doctype, R, S) end, Doctypes).

upgrade_doctype(Doctype, R, S) ->
    {ok, Json} = design_doctype_json_dtl:render(Doctype),
    VId = binary_to_list(jsn:get_value(<<"_id">>, Doctype)),
    {ok, updated} = couch:update(design, VId, Json, R, S),
    {ok, VJson} = couch:get_view_json(VId, "fieldsets", R, S),
    Fieldsets = [jsn:get_value(<<"value">>, X) || 
                    X <- jsn:get_value(<<"rows">>, VJson)],
    lists:map(fun (Fieldset) -> upgrade_fieldset(Fieldset, R, S) end, 
              Fieldsets).
  
upgrade_fieldset(Fieldset, R, S) ->
    {ok, Json} = design_fieldset_json_dtl:render(Fieldset),
    VId = binary_to_list(jsn:get_value(<<"_id">>, Fieldset)),
    {ok, updated} = couch:update(design, VId, Json, R, S),
    {ok, VJson} = couch:get_view_json(VId, "fields", R, S),
    Fields = [jsn:get_value(<<"value">>, X) || 
                 X <- jsn:get_value(<<"rows">>, VJson)],
    lists:map(fun (Field) -> upgrade_field(Field, R, S) end, Fields).

upgrade_field(Field, R, S) ->
    {ok, Json} = design_field_json_dtl:render(Field),
    VId = binary_to_list(jsn:get_value(<<"_id">>, Field)),
    {ok, updated} = couch:update(design, VId, Json, R, S).
