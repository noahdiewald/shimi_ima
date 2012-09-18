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

%%% @copyright 2012 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>

%%% @doc This module centralizes calls to couch:get_view_json/4 and
%%% couch:get_view_json/5

-module(q).

-compile(export_all).

-include_lib("config.hrl").
-include_lib("types.hrl").
-include_lib("webmachine/include/webmachine.hrl").

fieldsets(Doctype, R, S) ->
    couch:get_view_json(Doctype, "fieldsets_simple", R, S).

doctypes(R, S) ->
    couch:get_view_json("doctypes", "all", R, S).

doctypes(true, R, S) ->
    QS = view:to_string(#vq{include_docs = true}),
    couch:get_view_json("doctypes", "all", QS, R, S).

head_charseqs(Doctype, R, S) ->
    DT = list_to_binary(Doctype),
    VQ = #vq{startkey =  [<<"_head-charseq">>, DT, 0],
             endkey = [<<"_head-charseq">>, DT, []],
             include_docs = true},
    QS = view:to_string(VQ),
    couch:get_view_json("fieldsets", "all", QS, R, S).
    

charseqs(R, S) ->
    couch:get_view_json("charseqs", "all", R, S).

index(Id, R, S) ->
    couch:get_view_json(Id, "index", R, S).

indexes_options(R, S) ->
    couch:get_view_json("indexes", "options", R, S).

all_fieldset_for_doctype(Doctype, R, S) ->
    all_fieldset_for_doctype(Doctype, true, R, S).

all_fieldset_for_doctype(Doctype, Include, R, S) when is_list(Doctype) ->
    all_fieldset_for_doctype(list_to_binary(Doctype), Include, R, S);
all_fieldset_for_doctype(DT, Include, R, S) when is_binary(DT) ->
    VQ = #vq{startkey = [DT, <<"">>],
             endkey = [DT, []],
             include_docs = Include},
    QS = view:to_string(VQ),
    couch:get_view_json("fieldsets", "all", QS, R, S).

all_fields_for_fieldset(Doctype, Fieldset, R, S) when is_list(Doctype) ->
    all_fields_for_fieldset(Doctype, Fieldset, true, R, S).

all_fields_for_fieldset(Doctype, Fieldset, Include, R, S) 
  when is_list(Doctype) ->
    all_fields_for_fieldset(list_to_binary(Doctype), Fieldset, Include, R, S);
all_fields_for_fieldset(DT, Fieldset, Include, R, S) when is_list(Fieldset) ->
    all_fields_for_fieldset(DT, list_to_binary(Fieldset), Include, R, S);
all_fields_for_fieldset(DT, FS, Include, R, S) 
  when is_binary(DT), is_binary(FS) ->
    VQ = #vq{startkey = [DT, FS, <<"fieldset-field">>, <<"">>],
             endkey = [DT, FS, <<"fieldset-field">>, 0],
             descending = true,
             include_docs = Include},
    QS = view:to_string(VQ),
    couch:get_view_json("fieldsets", "all", QS, R, S).
    

altered_startkey(Id, R, S) ->
    couch:get_view_json(sortkeys, Id, "index", R, S).

search(Doctype, Field, R, S) ->
    VQ = #vq{startkey = [Doctype, Field, []],
             endkey = [Doctype, Field, <<"">>],
             descending = true},
    QS = view:to_string(VQ),
    couch:get_view_json("fields", "search", QS, R, S).
