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
  touch_all/3
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").
-include_lib("include/types.hrl").

%% @doc If configuration has changed, it may be desireable to update
%% previously saved documents. This will update all documents of a certain
%% doctype using the latest configuration settings.

-spec touch_all(Doctype :: string(), R :: utils:reqdata(), S :: any()) -> Conflicts :: jsn:json_term().
touch_all(Doctype, R, S) ->
  {ok, AllDocs} = couch:get_view_json(Doctype, "alldocs", R, S),
  Updated = [touch(jsn:get_value(<<"value">>, Row), R, S) || Row <- jsn:get_value(<<"rows">>, AllDocs)],
  BulkDocs = [{<<"docs">>, Updated}],
  Touched = couch:bulk_update(BulkDocs, R, S),
  F = fun ([{<<"id">>, _}, {<<"error">>, <<"conflict">>}, {<<"reason">>, <<"Document update conflict.">>}]) -> true;
          ([{<<"id">>, _}, {<<"rev">>, _}]) -> false
  end,
  lists:filter(F, jsn:get_value(<<"docs">>, Touched)).

-spec touch(Document :: jsn:json_term(), R :: utils:reqdata(), S :: any()) -> Document2 :: jsn:json_term().
touch(D, R, S) ->
  Doc = from_json(D),
  Doc2 = Doc#document{fieldsets=[fieldset:touch(FS, R, S) || FS <- Doc#document.fieldsets]},
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
    doctype = jsn:get_value(<<"doctype">>, Json),
    description = jsn:get_value(<<"description">>, Json),
    fieldsets = [fieldset:from_json(doc, X) || X <-  jsn:get_value(<<"fieldsets">>, Json)]
  }.

%% @doc Convert a document() record to a jsn:json_term() document.

-spec to_json(D :: document()) -> Json :: jsn:json_term().
to_json(D) ->
  [{<<"_id">>, D#document.id},
  {<<"_rev">>, D#document.rev},
  {<<"description">>, D#document.description},
  {<<"doctype">>, D#document.doctype},
  {<<"fieldsets">>,[fieldset:to_json(doc, X) || X <- D#document.fieldsets]}].
