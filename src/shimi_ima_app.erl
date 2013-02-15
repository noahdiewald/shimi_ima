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
%%% @doc Callbacks for the shimi_ima application.

-module(shimi_ima_app).
-author('Noah Diewald <noah@diewald.me>').

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    Opts = [],
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/", page_resource, [{target, main}|Opts]},
                                             {"/projects", project_resource, [{target, main}|Opts]},
                                             {"/projects/index", project_resource, [{target, index}|Opts]},
                                             {"/projects/:id", project_resource, [{target, identifier}|Opts]},
                                             {"/projects/:project/index_tool", index_tool_resource, [{target, main}|Opts]},
                                             {"/projects/:project/indexes", index_resource, [{target, index}|Opts]},
                                             {"/projects/:project/indexes/condition", index_resource, [{target, condition}|Opts]},
                                             {"/projects/:project/indexes/:id", index_resource, [{target, identifier}|Opts]},
                                             {"/projects/:project/indexes/view", index_resource, [{target, view}|Opts]},
                                             {"/projects/:project/config", config_resource, [{target, main}|Opts]},
                                             {"/projects/:project/config/upgrade", config_resource, [{target, upgrade}|Opts]},
                                             {"/projects/:project/config/doctypes", config_doctype_resource, [{target, index}|Opts]},
                                             {"/projects/:project/config/doctypes/:id", config_doctype_resource, [{target, identifier}|Opts]},
                                             {"/projects/:project/config/doctypes/touch", config_doctype_resource, [{target, touch}|Opts]},
                                             {"/projects/:project/config/doctypes/fieldsets", config_fieldset_resource, [{target, index}|Opts]},
                                             {"/projects/:project/config/doctypes/fieldsets/:id", config_fieldset_resource, [{target, identifier}|Opts]},
                                             {"/projects/:project/config/doctypes/fieldsets/fields", config_field_resource, [{target, index}|Opts]},
                                             {"/projects/:project/config/doctypes/fieldsets/fields/:id", config_field_resource, [{target, identifier}|Opts]},
                                             {"/projects/:project/config/charseqs", config_charseq_resource, [{target, index}|Opts]},
                                             {"/projects/:project/config/charseqs/:id", config_charseq_resource, [{target, identifier}|Opts]},
                                             {"/projects/:project/doctypes/documents", document_resource, [{target, main}|Opts]},
                                             {"/projects/:project/doctypes/documents/index", document_resource, [{target, index}|Opts]},
                                             {"/projects/:project/doctypes/documents/search", document_resource, [{target, search}|Opts]},
                                             {"/projects/:project/doctypes/documents/edit", document_resource, [{target, edit}|Opts]},
                                             {"/projects/:project/doctypes/documents/:id", document_resource, [{target, identifier}|Opts]},
                                             {"/projects/:project/doctypes/documents/:id/:rev", document_resource, [{target, revision}|Opts]},
                                             {"/projects/:project/doctypes/worksheets", document_resource, [{target, worksheets_get}|Opts]},
                                             {"/projects/:project/doctypes/worksheets/:id", document_resource, [{target, worksheets_put}|Opts]},
                                             {"/projects/:project/doctypes", doctype_resource, [{target, index}|Opts]},
                                             {"/projects/:project/doctypes/:doctype", doctype_resource, [{target, identifier}|Opts]},
                                             {"/projects/:project/doctypes/indexes", index_resource, [{target, index}|Opts]},
                                             {"/projects/:project/doctypes/fieldsets", fieldset_resource, [{target, index}|Opts]},
                                             {"/projects/:project/doctypes/fieldsets/:id", fieldset_resource, [{target, identifier}|Opts]},
                                             {"/projects/:project/doctypes/fieldsets/fields", field_resource, [{target, index}|Opts]},
                                             {"/projects/:project/doctypes/fieldsets/fields/:id", field_resource, [{target, identifier}|Opts]},
                                             {"/projects/:project/file_manager", file_manager_resource, [{target, main}|Opts]},
                                             {"/projects/:project/file_manager/index", file_manager_resource, [{target, index}|Opts]},
                                             {"/projects/:project/file_manager/upload", file_manager_resource, [{target, upload}|Opts]},
                                             {"/projects/:project/file_manager/list_dirs/[...]", file_manager_resource, [{target, list_dirs}|Opts]},
                                             {"/projects/:project/file_manager/list_files/[...]", file_manager_resource, [{target, list_files}|Opts]},
                                             {"/projects/:project/file_manager/files/[...]", file_manager_resource, [{target, path}|Opts]},
                                             {"/projects/:project/file_manager/:id", file_manager_resource, [{target, identifier}|Opts]},
                                             {"/[...]", cowboy_static, [{directory, {priv_dir, shimi_ima, [<<"www">>]}},
                                                                        {mimetypes, {fun mimetypes:path_to_mimes/2, default}},
                                                                        {etag, {attributes, [filepath, filesize, inode, mtime]}}]}
                                            ]}]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8000}], [{env, [{dispatch, Dispatch}]}]),
    shimi_ima:start_link().

stop(_State) ->
    ok.
