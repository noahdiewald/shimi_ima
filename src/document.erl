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
%%% @doc This module contains functions for manipulating documents

-module(document).

-export([
  from_json/1,
  set_sortkeys/3,
  to_json/1,
  touch_all/2,
  touch_all/3
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").
-include_lib("include/types.hrl").

%% @doc If configuration has changed, it may be desireable to update
%% previously saved documents. This will update all documents of a certain
%% doctype using the latest configuration settings.

-spec touch_all(R :: utils:reqdata(), S :: any()) -> Conflicts :: jsn:json_term().
touch_all(R, S) ->
  touch_all(wrq:path_info(id, R), R, S).

-spec touch_all(Id :: string(), R :: utils:reqdata(), S :: any()) -> Conflicts :: jsn:json_term().
touch_all(Id, R, S) ->
  {ok, AllDocs} = couch:get_view_json(Id, "quickdocs", R, S),
  Updated = [touch(jsn:get_value(<<"value">>, Row), R, S) || Row <- jsn:get_value(<<"rows">>, AllDocs)],
  BulkDocs = [{<<"docs">>, Updated}],
  Touched = couch:bulk_update(BulkDocs, R, S),
  F = fun ([{<<"ok">>, true}|_]) -> false;
          (_) -> true
  end,
  lists:filter(F, Touched).

-spec touch(Document :: jsn:json_term(), R :: utils:reqdata(), S :: any()) -> Document2 :: jsn:json_term().
touch(D, R, S) ->
  Doc = from_json(D),
  Doc2 = Doc#document{
    prev = Doc#document.rev,
    fieldsets = [fieldset:touch(FS, R, S) || FS <- Doc#document.fieldsets]},
  to_json(Doc2).
  
%% @doc Set the sortkeys for the fields in the document. 

-spec set_sortkeys(jsn:json_term(), R :: utils:reqdata(), S :: any()) -> jsn:json_term().
set_sortkeys(Doc, R, S) when is_list(Doc) -> 
  jsn:set_value(<<"fieldsets">>, fieldset:set_sortkeys(jsn:get_value(<<"fieldsets">>, Doc), R, S), Doc);
set_sortkeys(D=#document{}, R, S) ->
  D#document{fieldsets=fieldset:set_sortkeys(D#document.fieldsets, R, S)}.

%% @doc Convert a jsn:json_term() document to a document() record.

-spec from_json(Json :: jsn:json_term()) -> document().
from_json(Json) ->
  #document{
    id = jsn:get_value(<<"_id">>, Json),
    rev = jsn:get_value(<<"_rev">>, Json),
    prev = proplists:get_value(<<"prev_">>, Json, null),
    doctype = jsn:get_value(<<"doctype">>, Json),
    description = proplists:get_value(<<"description">>, Json, null),
    created_at = decode_date(proplists:get_value(<<"created_at_">>, Json, null)),
    updated_at = decode_date(proplists:get_value(<<"updated_at_">>, Json, null)),
    created_by = proplists:get_value(<<"created_by_">>, Json, null),
    updated_by = proplists:get_value(<<"updated_by_">>, Json, null),
    deleted = proplists:get_value(<<"deleted_">>, Json, false),
    fieldsets = [fieldset:from_json(doc, X) || X <-  jsn:get_value(<<"fieldsets">>, Json)]
  }.

%% @doc Convert a document() record to a jsn:json_term() document.

-spec to_json(D :: document()) -> Json :: jsn:json_term().
to_json(D) ->
  [{<<"_id">>, D#document.id},
  {<<"_rev">>, D#document.rev},
  {<<"description">>, D#document.description},
  {<<"doctype">>, D#document.doctype},
  {<<"created_at_">>, encode_date(D#document.created_at)},
  {<<"created_by_">>, D#document.created_by},
  {<<"updated_at_">>, encode_date(D#document.updated_at)},
  {<<"updated_by_">>, D#document.updated_by},
  {<<"prev_">>, D#document.prev},
  {<<"deleted_">>, D#document.deleted},
  {<<"fieldsets">>,[fieldset:to_json(doc, X) || X <- D#document.fieldsets]}].

-spec decode_date(jsn:json_term()) -> calendar:datetime().
decode_date(null) ->
  null;
decode_date(Json) ->
  case js_date:convert(binary_to_list(Json)) of
    bad_date -> null;
    Datetime -> Datetime
  end.
  
-spec encode_date(calendar:datetime() | null) -> jsn:json_term().
encode_date(null) ->
  null;
encode_date(Datetime) ->
  list_to_binary(js_date:to_string(Datetime)).
