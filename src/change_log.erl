%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
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
%%% @doc For adding entries to the change log.

-module(change_log).
-author('Noah Diewald <noah@diewald.me>').

-record(change, {
          id :: binary(),
          rev :: binary(),
          category :: change,
          change_type :: creation|update|deletion|restoration,
          changes :: jsn:json_term(),
          doctype :: binary(),
          document_id :: binary(),
          document_revision :: binary(),
          head_ids :: [binary()],
          head_values :: [binary()],
          user :: binary(),
          timestamp :: binary()}).

-export([
         created/4,
         deleted/4,
         restored/4,
         updated/4
        ]).

created(Data, Doctype, Project, S) ->
    create_change(creation, Data, Doctype, Project, S).

create_change(CType, Data, Doctype, Project, S) ->
    Timestamp = jsn:get_value(<<"timestamp">>, Data),
    User = jsn:get_value(<<"user">>, Data),
    DocId = jsn:get_value(<<"document_id">>, Data),
    DocRev = jsn:get_value(<<"document_revision">>, Data),
    Id = create_id(list_to_binary(Doctype), Timestamp),
    Changes = jsn:get_value(<<"changes">>, Data),
    HeadIds = jsn:get_value(<<"head_ids">>, Data),
    HeadValues = jsn:get_value(<<"head_values">>, Data),
    C = #change{
           id = Id,
           category = change,
           change_type = CType,
           changes = Changes,
           document_id = DocId,
           document_revision = DocRev,
           head_ids = HeadIds,
           head_values = HeadValues,
           user = User,
           timestamp = Timestamp
          },
    Json = to_json(C),
    {ok, _} = couch:update(binary_to_list(Id), Json, Project, [{admin, true}|S]),
    ok.

create_id(Doctype, Timestamp) ->
    T = timestamp_to_id_part(js_date:convert(binary_to_list(Timestamp))),
    U = list_to_binary(utils:uuid()),
    <<Doctype/binary,"-",T/binary,"-",U/binary>>.
 
deleted(Data, Doctype, Project, S) ->
    create_change(deletion, Data, Doctype, Project, S).

restored(Data, Doctype, Project, S) ->
    create_change(restoration, Data, Doctype, Project, S).

timestamp_to_id_part({ok, {{Y, M, D}, {H, MM, S}}}) ->
    iolist_to_binary(io_lib:format("~B~2..0B~2..0B~2..0B~2..0B~2..0B", [Y, M, D, H, MM, S])).

to_json(C) ->
    [{<<"timestamp">>, C#change.timestamp},
     {<<"_id">>, C#change.id},
     {<<"user">>, C#change.user},
     {<<"document_id">>, C#change.document_id},
     {<<"document_revision">>, C#change.document_revision},
     {<<"category">>, <<"change">>},
     {<<"head_ids">>, C#change.head_ids},
     {<<"head_values">>, C#change.head_values},
     {<<"changes">>, C#change.changes},
     {<<"change_type">>, C#change.change_type}].

updated(Data, Doctype, Project, S) ->
    create_change(update, Data, Doctype, Project, S).
