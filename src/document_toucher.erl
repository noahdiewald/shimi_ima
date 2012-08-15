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
%%% @doc This module contains functions for touching documents. In
%%% this context "touching" refers to a maintenance action where all
%%% documents are updated and made to conform to any changes in
%%% fieldsets, fields, charseqs, etc.

-module(document_toucher).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([
         touch_all/2,
         touch_all/3
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("config.hrl").
-include_lib("types.hrl").

%% @doc If configuration has changed, it may be desireable to update
%% previously saved documents. This will update all documents of a
%% certain doctype using the latest configuration settings.
-spec touch_all(R :: utils:reqdata(), S :: any()) -> Conflicts :: jsn:json_term().
touch_all(R, S) ->
    touch_all(wrq:path_info(id, R), R, S).

-spec touch_all(Id :: string(), R :: utils:reqdata(), S :: any()) -> Conflicts :: jsn:json_term().
touch_all(Id, R, S) ->
    Tid = ets:new(touch_documents, [public]),
    error_logger:info_report([{touch_all, starting}]),
    S1 = [{table_id, Tid}|S],
    {ok, AllDocs} = couch:get_view_json(Id, "quickdocs", R, S),
    Rows = jsn:get_value(<<"rows">>, AllDocs),
    F = fun (Row) -> touch(jsn:get_value(<<"key">>, Row), R, S1) end,
    utils:peach(F, Rows, 5),
    error_logger:info_report([{touch_all, finished}]),
    true = ets:delete(Tid).

-spec touch(binary(), utils:reqdata(), any()) -> ok.
touch(Id, R, S) ->
    Json = touch_get_json(Id, R, S),
    Doc = document:from_json(Json),
    case couch:get_view_json(binary_to_list(Doc#document.doctype), "fieldsets",
                             R, S) of
        {ok, Fieldsets} ->
            FieldsetIds = [jsn:get_value(<<"id">>, X) || 
                              X <- jsn:get_value(<<"rows">>, Fieldsets)],
            Doc2 = document:to_json(
                     Doc#document{
                       prev = Doc#document.rev,
                       fieldsets = fieldset:touch_all(Doc#document.fieldsets, 
                                                      FieldsetIds, R, S)}),
            case couch:update(doc, binary_to_list(Id), 
                              jsn:encode(Doc2), R, S) of
                {ok, updated} -> ok;
                Unexpected ->
                    error_logger:error_report(
                      [{touch_document_update, {Unexpected, Doc2}}])
            end;
        {error, req_timedout} -> touch(Id, R, S)
    end.

-spec touch_get_json(binary(), utils:reqdata(), any()) -> json:json_term().
touch_get_json(Id, R, S) ->                
    {ok, Json} = couch:get_json(safer, binary_to_list(Id), R, S),
    Json.
  
