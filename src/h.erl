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
%%% @doc Helpers for resources. Generally a generic place to handle
%%% CRUD.

-module(h).
-author('Noah Diewald <noah@diewald.me>').

-export([
         accept_json/2,
         basic_info/4,
         charseq/1,
         charseq_data/2,
         create/3,
         create_attachment/5,
         delete/2,
         delete_project/2,
         doctype/1,
         doctype_data/2,
         exists/3,
         exists_id/2,
         exists_unless_post/2,
         exists_with_deps/3,
         extract_create_data/2,
         field/1,
         field_data/2,
         fieldset/1,
         fieldset_data/2,
         g/2,
         get/3,
         get/4,
         get_attachment/4,
         id/1,
         id_data/2,
         id_html/3,
         index/1,
         path_exists/2,
         path_exists/3,
         project/1,
         project_data/2,
         rev/1,
         rev_data/2,
         update/3,
         update_doctype_version/2
        ]).

-export_type([req_data/0, req_state/0, req_retval/0]).

-include_lib("types.hrl").

-type req_data() :: cowboy_req:req().
-type req_state() :: [{atom(), any()}].
-type couch_ret() :: couch:ret().
-type req_retval() :: {couch_ret(), req_data()}.

%% @doc Used by resources to remove some boilerplate.
-spec accept_json(req_data(), req_state()) -> {[{{binary(), binary(), list()}, from_json}], req_data(), req_state()}.
accept_json(R, S) -> {[{{<<"application">>, <<"json">>, '*'}, from_json}], R, S}.

%% @doc There is often some basic contextual information that is
%% needed by the client application after it makes a request. This
%% function will return this information. There are title arguments
%% used to send back information about how a page should be titled as
%% well.
%%
%% TODO: Remove the title stuff.
-spec basic_info(string(), string(), req_data(), req_state()) -> {jsn:json_term(), req_data()}.
basic_info(Title1, Title2, R, S) ->
    {{ok, ProjectData}, R1} = project_data(R, S),
    {{ok, DoctypeData}, R2} = doctype_data(R1, S),
    % TODO: use the data above?
    {Doctype, R3} = doctype(R2),
    {[{<<"project_info">>, ProjectData},
      {<<"doctype_info">>, DoctypeData},
      {<<"title">>, list_to_binary(Title1 ++ Doctype ++ Title2)},
      {<<"user">>, proplists:get_value(user, S)}], R3}.

%% @doc Return the value of the 'charseq' binding.
-spec charseq(req_data()) -> {string(), req_data()}.
charseq(R) ->
    {CharseqBin, R1} = cowboy_req:binding(charseq, R),
    {list_or_undefined(CharseqBin), R1}.

%% @doc Retrieve the charseq item from the database using the value of
%% the 'charseq' binding.
-spec charseq_data(req_data(), req_state()) -> req_retval().
charseq_data(R, S) ->
    {[Charseq, Project], R1} = g([charseq, project], R),
    {get(Charseq, Project, S), R1}.

%% @doc Create a database item.
-spec create(jsn:json_term(), req_data(), req_state()) -> {true, req_data(), req_state()} | req_data().
create(Json, R, S) ->
    {Project, R1} = project(R),
    case couch:create(Json, Project, S) of
        {ok, _} -> 
            {{true, proplists:get_value(create_path, S)}, R1, S};
        {forbidden, Message} ->
            {ok, R2} = cowboy_req:reply(403, [], Message, R1),
            {halt, R2, S}
    end.

%% @doc Add an attachment to a database item.
-spec create_attachment(string(), string(), binary(), req_data(), req_state()) -> {true, req_data(), req_state()} | {ok, req_data()}.
create_attachment(Id, Name, Content, R, S) ->
    {Project, R1} = project(R),
    case couch:update_raw(Id ++ "/" ++ Name, Content, Project, S) of
        {ok, _} ->
            Msg = <<"File Uploaded">>,
            Msg1 = jsn:encode([{<<"message">>, Msg}, {<<"status">>, <<"success">>}]),
            {ok, R2} = cowboy_req:reply(200, [], Msg1, R1),
            {halt, R2, S};
        {forbidden, Message} ->
            {ok, R2} = cowboy_req:reply(403, [], Message, R1),
            {halt, R2, S}
    end.

%% @doc Delete a database item.
-spec delete(req_data(), req_state()) -> {true, req_data(), req_state()} | {ok, req_data()}.
delete(R, S) ->
    {[Project, Rev, Id], R1} = g([project, rev, id], R),
    case couch:delete(Id, Rev, Project, S) of
        {ok, deleted} -> 
            {true, R1, S};
        {error, conflict} ->
            Msg = <<"This document has been updated or deleted by another user.">>,
            Msg1 = jsn:encode([{<<"message">>, Msg}]),
            {ok, R2} = cowboy_req:reply(409, [], Msg1, R1),
            {halt, R2, S}
    end.

%% @doc Delete a project aka a database.
-spec delete_project(req_data(), req_state()) -> {true, req_data(), req_state()} | {ok, req_data()}.
delete_project(R, S) ->
    {Id, R1} = id(R),
    {ok, Json} = get(Id, "shimi_ima", S),
    Rev = jsn:get_value(<<"_rev">>, Json),
    case couch:delete(Id, binary_to_list(Rev), "shimi_ima", S) of
        {ok, deleted} ->
            {ok, deleted} = couch:rm_db("project-" ++ Id),
            {true, R1, S};
        {error, conflict} ->
            Msg = <<"This project has been updated or deleted by another user.">>,
            Msg1 = jsn:encode([{<<"message">>, Msg}]),
            {ok, R2} = cowboy_req:reply(409, [], Msg1, R1),
            {halt, R2, S}
    end.

%% @doc Return the value of the 'doctype' binding.
-spec doctype(req_data()) -> string().
doctype(R) ->
    {DoctypeBin, R1} = cowboy_req:binding(doctype, R),
    {list_or_undefined(DoctypeBin), R1}.

%% @doc Retrieve a doctype item from the database using the 'doctype'
%% binding.
-spec doctype_data(req_data(), req_state()) -> req_retval().
doctype_data(R, S) ->
    {[Project, Doctype], R1} = g([project, doctype], R),
    {get(Doctype, Project, S), R1}.

%% @doc Check for the existence of a database item.
-spec exists(string()|null|undefined, req_data(), req_state()) -> {boolean(), req_data(), req_state()}.
exists(Id, R, S) ->
    {Project, R1} = project(R),
    {couch:exists(Id, Project, S), R1, S}.

%% @doc Check for the existence of the database item specified by the
%% 'id' binding.
-spec exists_id(req_data(), req_state()) -> {boolean(), req_data(), req_state()}.
exists_id(R, S) ->
    {Id, R1} = id(R),
    exists(Id, R1, S).

%% @doc If the request is a POST, return false, otherwise return true.
-spec exists_unless_post(req_data(), req_state()) -> {boolean(), req_data(), req_state()}.
exists_unless_post(R, S) ->
    case cowboy_req:method(R) of
        {<<"POST">>, R1} -> {false, R1, S};
        {_, R1} -> {true, R1, S}
    end.

%% @doc Used to report a 404 when an item's dependencies do not exist
%% in the database.
%%
%% TODO: This should be removed and a better return value than 404
%% used.
-spec exists_with_deps([atom()|list()], req_data(), req_state()) -> {boolean(), req_data(), req_state()}.
exists_with_deps([], R, S) ->
    exists_unless_post(R, S);
exists_with_deps([Binding|Rest], R, S) when is_atom(Binding), Binding /= undefined ->
    {Value, R1} = ?MODULE:Binding(R),
    exists_with_deps([Value|Rest], R1, S);
exists_with_deps([BValue|_], R, S) when BValue =:= undefined ->
    {ok, R1} = cowboy_req:reply(404, [], <<>>, R),
    {halt, R1, S};
exists_with_deps([BValue|Rest], R, S) when is_list(BValue) ->
    case exists(BValue, R, S) of
        {true, R1, S} -> exists_with_deps(Rest, R1, S);
        _ ->
            {ok, R1} = cowboy_req:reply(404, [], <<>>, R),
            {halt, R1, S}
    end.

%% @doc Extract JSON from the request body and create a new database
%% item from it.
-spec extract_create_data(req_data(), req_state()) -> {req_data(), req_state()}.
extract_create_data(R, S) ->
    {ok, Body, R1} = cowboy_req:body(R),
    Json = jsn:decode(Body),
    {Id, Json1} = case jsn:get_value(<<"_id">>, Json) of
                      undefined -> 
                          GenId = list_to_binary(utils:uuid()),
                          {GenId, jsn:set_value(<<"_id">>, GenId, Json)};
                      IdBin -> {IdBin, Json}
                  end,
    {R1, [{create_path, <<"/", Id/binary>>}, {posted_json, Json1}|S]}.

%% @doc Return either the value of the binding 'field' or return a
%% query string value for the key 'field' if the binding is undefined.
-spec field(req_data()) -> {string() | undefined, req_data()}.
field(R) ->
    {FieldBin, R1} = cowboy_req:binding(field, R),
    case FieldBin of
        undefined ->
            {Field, R2} = cowboy_req:qs_val(<<"field">>, R1),
            {list_or_undefined(Field), R2};
        FieldBin when is_binary(FieldBin) ->
            {binary_to_list(FieldBin), R1}
    end.

%% @doc Retrieve the field item specified by field/1.
-spec field_data(req_data(), req_state()) -> req_retval().
field_data(R, S) ->
    {[Project, Field], R1} = g([project, field], R),
    {get(Field, Project, S), R1}.

%% @doc Return the value of the 'fieldset' binding.
-spec fieldset(req_data()) -> {string(), req_data()}.
fieldset(R) ->
    {FieldsetBin, R1} = cowboy_req:binding(fieldset, R),
    {list_or_undefined(FieldsetBin), R1}.

%% @doc Retrieve the fieldset item specified by the 'fieldset'
%% binding.
-spec fieldset_data(req_data(), req_state()) -> req_retval().
fieldset_data(R, S) ->
    {[Project, Fieldset], R1} = g([project, fieldset], R),
    {get(Fieldset, Project, S), R1}.

%% @doc Gather a bunch of values from the req_data() when given a list
%% of atom() keys.
-spec g([atom()], req_data()) -> {[string() | undefined], req_data()}.
g(Keys, R) ->
  F = fun (X, {RX, Acc}) ->
    {Val, RY} = ?MODULE:X(RX),
    {RY, [Val|Acc]}
  end,
  {R1, Vals} = lists:foldl(F, {R, []}, Keys),
  {lists:reverse(Vals), R1}.

%% @doc Retrieve an item from the database.
-spec get(string(), string(), req_state()) -> couch_ret().
get(Id, Project, S) ->
    couch:get(Id, Project, S).

%% @doc Retrieve a specific revision of an item from the database.
-spec get(string(), string(), string(), req_state()) -> couch_ret().
get(Id, Rev, Project, S) ->
    couch:get(Id, Rev, Project, S).

%% @doc Retrieve an attachment from a database document.
-spec get_attachment(string(), string(), string(), req_state()) -> couch_ret().
get_attachment(Id, Name, Project, S) ->
    couch:get_attachment(Id, Name, Project, S).

%% @doc Return the value of the 'id' binding.
-spec id(req_data()) -> string().
id(R) ->
    {IdBin, R1} = cowboy_req:binding(id, R),
    {list_or_undefined(IdBin), R1}.

%% @doc Retrieve the item from the database with the id specified in
%% the 'id' binding.
-spec id_data(req_data(), req_state()) -> req_retval().
id_data(R, S) ->
    {[Project, Id], R1} = g([project, id], R),
    {get(Id, Project, S), R1}.

%% @doc Retrieve the item from the database with the id specified in
%% the 'id' binding. Then render it using the supplied templated
%% name. The assumption is that it will be an HTML template but any
%% erlydtl template will due.
-spec id_html(atom(), req_data(), req_state()) -> {iolist(), req_data(), req_state()}.
id_html(Template, R, S) ->
    {{ok, Json}, R1} = h:id_data(R, S), 
    {ok, Html} = render:render(Template, Json),
    {Html, R1, S}.

%% @doc When a user created view index is requested in a query string,
%% return the identifier for the index as a string. Otherwise, return
%% undefined.
-spec index(req_data()) -> {string() | undefined, req_data()}.
index(R) ->
    {IndexBin, R1} = cowboy_req:qs_val(<<"index">>, R),
    {list_or_undefined(IndexBin), R1}.

%% @doc Convert a binary to a list, unless the given value isn't a
%% binary, in which case, return undefined.
-spec list_or_undefined(binary()|any()) -> string()|undefined.
list_or_undefined(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
list_or_undefined(_) ->
    undefined.

%% @doc Determine if the "path" exists based on the value of
%% cowboy_req:path_info/1. The path is actually a key or partial key
%% for a document containing an attachment.
-spec path_exists(req_data(), req_state()) -> {boolean(), req_data(), req_state()}.
path_exists(R, S) ->
    {Path, R1} = cowboy_req:path_info(R),
    path_exists(Path, R1, S).

%% @doc Determine if the given "path" exists. The path is actually a
%% key or partial key for a document containing an attachment.
-spec path_exists([string()|binary()], req_data(), req_state()) -> {boolean(), req_data(), req_state()}.
path_exists(Path, R, S) ->
    {Project, R1} = project(R),
    {ok, Json} = q:full_path([{<<"key">>, Path}], Project, S),
    Total = length(proplists:get_value(<<"rows">>, Json)),
    {Total > 0, R1, S}.

%% @doc Return the value of the 'project' binding.
-spec project(req_data()) -> {string(), req_data()}.
project(R) ->
    {ProjectBin, R1} = cowboy_req:binding(project, R),
    {list_or_undefined(ProjectBin), R1}.

%% @doc Retrieve the project item from the shimi_ima database, where
%% project information is centrally stored.
-spec project_data(req_data(), req_state()) -> req_retval().
project_data(R, S) ->
    {Project, R1} = project(R),
    {get(Project -- "project-", "shimi_ima", S), R1}.

%% @doc Like field/1 but for a document revision specified as 'rev'.
-spec rev(req_data()) -> {string(), req_data()}.
rev(R) ->
    {RevBin, R1} = cowboy_req:binding(rev, R),
    case RevBin of
        undefined ->
            {Rev, R2} = cowboy_req:qs_val(<<"rev">>, R1),
            {list_or_undefined(Rev), R2};
        RevBin when is_binary(RevBin) ->
            {binary_to_list(RevBin), R1}
    end.

%% @doc Using the return values of project/1, rev/1 and id/1 retrieve
%% a specific document revision from the database.
-spec rev_data(req_data(), req_state()) -> req_retval().
rev_data(R, S) ->
    {[Project, Rev, Id], R1} = g([project, rev, id], R),
    {get(Id, Rev, Project, S), R1}.

%% @doc Update a database item.
-spec update(jsn:json_term(), req_data(), req_state()) -> {true, req_data(), req_state()} | {ok, req_data()}.
update(Json, R, S) ->
    {[Project, Rev, Id], R1} = g([project, rev, id], R),
    Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
    Json2 = jsn:set_value(<<"_rev">>, list_to_binary(Rev), Json1),
    case couch:update(Id, Json2, Project, S) of
        {ok, _} -> 
            {true, R1, S};
        {forbidden, Message} ->
            {ok, R2} = cowboy_req:reply(403, [], Message, R1),
            {halt, R2, S};
        {error, conflict} ->
            Msg = <<"This document has been updated or deleted by another user.">>,
            Msg1 = jsn:encode([{<<"message">>, Msg}]),
            {ok, R2} = cowboy_req:reply(409, [], Msg1, R1),
            {halt, R2, S}
    end.

%% @doc Used to bump a doctype revision when some dependent item is
%% updated.
%%
%% TODO: find a better mechanism for the client to keep track of
%% dependent items.
-spec update_doctype_version(req_data(), req_state()) -> req_retval().
update_doctype_version(R, S) ->
    {[Project, Doctype], R1} = g([project, doctype], R),
    {ok, Json} = get(Doctype, Project, S),
    Id = jsn:get_value(<<"_id">>, Json),
    {ok, Data} = couch:update(binary_to_list(Id), Json, Project, S),
    Rev = jsn:get_value(<<"rev">>, Data),
    {{ok, Id, Rev}, R1}.
