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

-include_lib("types.hrl").
-include_lib("webmachine/include/webmachine.hrl").

all_docs(Qs, Project, S) ->
    couch:get_view_json(Qs, Project, S).

charseqs(R, S) ->
    Qs = view:normalize_vq(wrq:req_qs(R)),
    couch:get_view_json("shimi_ima", "all_charseqs", Qs, h:project(R), S).

doctypes(R, S) ->
    Qs = view:normalize_vq(wrq:req_qs(R)),
    couch:get_view_json("shimi_ima", "all_doctypes", Qs, h:project(R), S).

doctypes(true, R, S) ->
    Qs = view:to_string(#vq{include_docs = true}),
    couch:get_view_json("shimi_ima", "all_doctypes", Qs, h:project(R), S).

fieldset(Doctype, Project, S) ->
    fieldset(Doctype, true, Project, S).

fieldset(Doctype, Include, Project, S) ->
    DT = list_to_binary(Doctype),
    VQ = #vq{startkey = [DT, <<"">>],
	     endkey = [DT, []],
	     include_docs = Include},
    Qs = view:to_string(VQ),
    couch:get_view_json("shimi_ima", "all_fieldsets", Qs, Project, S).

field(Doctype, Fieldset, Project, S) ->
    field(Doctype, Fieldset, true, Project, S).

field(Doctype, Fieldset, Include, Project, S) ->
    FS = list_to_binary(Fieldset),
    DT = list_to_binary(Doctype),
    VQ = #vq{startkey = [DT, FS, <<"fieldset-field">>, 0],
             endkey = [DT, FS, <<"fieldset-field">>, true],
             descending = true,
             include_docs = Include},
    Qs = view:to_string(VQ),
    couch:get_view_json("shimi_ima", "all_fieldsets", Qs, Project, S).

files(R, S) ->
    VQ = view:from_reqdata(R),
    Qs = view:to_string(VQ),
    couch:get_view_json("shimi_ima", "by_path", Qs, h:project(R), S).

head_charseqs(Doctype, Project, S) ->
    DT = list_to_binary(Doctype),
    VQ = #vq{startkey =  [<<"_head-charseq">>, DT, 0],
             endkey = [<<"_head-charseq">>, DT, []],
             include_docs = true},
    Qs = view:to_string(VQ),
    couch:get_view_json("shimi_ima", "all_fieldsets", Qs, Project, S).

index(Id, R, S) ->
    Qs = view:normalize_sortkey_vq(Id, wrq:req_qs(R), h:project(R), S),
    index(Id, Qs, h:project(R), S).

index(Id, Qs, Project, S) ->
    couch:get_view_json(Id, "index", Qs, Project, S).

index_design(Id, Project, S) ->
    Qs = view:to_string(#vq{key = list_to_binary(Id)}),
    couch:get_view_json("shimi_ima", "user_indexes", Qs, Project, S).

indexes_options(R, S) ->
    Qs = case h:doctype(R) of
             undefined ->
                 view:normalize_vq(wrq:req_qs(R));
             Doctype ->
                 VQ = #vq{startkey = [list_to_binary(Doctype), []],
                          endkey = [list_to_binary(Doctype), <<"">>],
                          descending = true},
                 view:to_string(VQ)
         end,
    couch:get_view_json("shimi_ima", "options", Qs, h:project(R), S).

search(Doctype, Field, Project, S) ->
    VQ = #vq{startkey = [list_to_binary(Doctype), Field, []],
             endkey = [list_to_binary(Doctype), Field, <<"">>],
             descending = true},
    Qs = view:to_string(VQ),
    couch:get_view_json("shimi_ima", "search", Qs, Project, S).
