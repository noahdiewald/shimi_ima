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

document_index(Doctype, R, S) ->
    F = fun (X) ->
        [_, Y] = jsn:get_value(<<"key">>, X),
        jsn:set_value(<<"key">>, Y, X)
    end,
    {ok, Json} = couch:get_view_json(sortkeys, "shimi_ima", "all_documents", Doctype, R, S),
    {ok, jsn:set_value(<<"rows">>, lists:map(F, jsn:get_value(<<"rows">>, Json)), Json)}.

charseqs(R, S) ->
    couch:get_view_json("shimi_ima", "all_charseqs", R, S).

doctypes(R, S) ->
    couch:get_view_json("shimi_ima", "all_doctypes", R, S).

doctypes(true, R, S) ->
    QS = view:to_string(#vq{include_docs = true}),
    couch:get_view_json("shimi_ima", "all_doctypes", QS, R, S).

fieldset(Doctype, R, S) ->
    fieldset(Doctype, true, R, S).

fieldset(Doctype, Include, R, S) when is_list(Doctype) ->
    fieldset(list_to_binary(Doctype), Include, R, S);
fieldset(DT, Include, R, S) when is_binary(DT) ->
    VQ = #vq{startkey = [DT, <<"">>],
             endkey = [DT, []],
             include_docs = Include},
    QS = view:to_string(VQ),
    couch:get_view_json("shimi_ima", "all_fieldsets", QS, R, S).

field(Doctype, Fieldset, R, S) ->
    field(Doctype, Fieldset, true, R, S).

field(Doctype, Fieldset, Include, R, S) 
  when is_list(Doctype) ->
    field(list_to_binary(Doctype), Fieldset, Include, R, S);
field(DT, Fieldset, Include, R, S) when is_list(Fieldset) ->
    field(DT, list_to_binary(Fieldset), Include, R, S);
field(DT, FS, Include, R, S) 
  when is_binary(DT), is_binary(FS) ->
    VQ = #vq{startkey = [DT, FS, <<"fieldset-field">>, 0],
             endkey = [DT, FS, <<"fieldset-field">>, true],
             descending = true,
             include_docs = Include},
    QS = view:to_string(VQ),
    couch:get_view_json("shimi_ima", "all_fieldsets", QS, R, S).

files(R, S) ->
    VQ = view:from_reqdata(R),
    QS = view:to_string(VQ),
    couch:get_view_json("shimi_ima", "by_path", QS, R, S).

head_charseqs(Doctype, R, S) ->
    DT = list_to_binary(Doctype),
    VQ = #vq{startkey =  [<<"_head-charseq">>, DT, 0],
             endkey = [<<"_head-charseq">>, DT, []],
             include_docs = true},
    QS = view:to_string(VQ),
    couch:get_view_json("shimi_ima", "all_fieldsets", QS, R, S).

indexes_options(R, S) ->
    couch:get_view_json("shimi_ima", "options", R, S).

search(Doctype, Field, R, S) ->
    VQ = #vq{startkey = [Doctype, Field, []],
             endkey = [Doctype, Field, <<"">>],
             descending = true},
    QS = view:to_string(VQ),
    couch:get_view_json("shimi_ima", "search", QS, R, S).

%% @doc Take an id for a saved index and return a JSON term. This term
%% will contain either the results of querying a view or will contain
%% only [{<<"rows">>, []}] if no conditions have been defined for the
%% query.
-spec user_index(string(), utils:reqdata(), any()) -> jsn:json_term().
user_index(IndexId, R, S) ->
    case couch:get_view_json(sortkeys, IndexId, "index", R, S) of
        {ok, Json} -> {ok, Json};
        _ -> {ok, [{<<"rows">>, []}]}
    end.

index(Id, R, S) ->
    couch:get_view_json(Id, "index", R, S).
