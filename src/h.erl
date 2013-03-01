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
%%% @doc Helpers for resources.

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

-spec accept_json(req_data(), req_state()) -> {[{{binary(), binary(), list()}, from_json}], req_data(), req_state()}.
accept_json(R, S) ->
    {[{{<<"application">>, <<"json">>, []}, from_json},
      {{<<"application">>, <<"json">>, [{<<"charset">>, <<"UTF-8">>}]}, from_json}], R, S}.
    
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
    
-spec charseq(req_data()) -> {string(), req_data()}.
charseq(R) ->
    {CharseqBin, R1} = cowboy_req:binding(charseq, R),
    {list_or_undefined(CharseqBin), R1}.

-spec charseq_data(req_data(), req_state()) -> req_retval().
charseq_data(R, S) ->
    {[Charseq, Project], R1} = g([charseq, project], R),
    {get(Charseq, Project, S), R1}.

-spec create(jsn:json_term(), req_data(), req_state()) -> {true, req_data(), req_state()} | req_data().
create(Json, R, S) ->
    {Project, R1} = project(R),
    case couch:create(Json, Project, S) of
        {ok, created} -> 
            {true, R1, S};
        {forbidden, Message} ->
            {ok, R2} = cowboy_req:reply(403, [], Message, R1),
            R2
    end.

-spec create_attachment(string(), string(), binary(), req_data(), req_state()) -> {true, req_data(), req_state()} | {ok, req_data()}.
create_attachment(Id, Name, Content, R, S) ->
    {Project, R1} = project(R),
    case couch:update(Id ++ "/" ++ Name, Content, Project, S) of
        {ok, updated} -> 
            {ok, R2} = cowboy_req:reply(200, [], <<"File Uploaded">>, R1),
            R2;
        {forbidden, Message} ->
            {ok, R2} = cowboy_req:reply(403, [], Message, R1),
            R2
    end.

-spec delete(req_data(), req_state()) -> {true, req_data(), req_state()} | {ok, req_data()}.
delete(R, S) ->
    {[Project, Rev, Id], R1} = g([project, rev, id], R),
    case couch:delete(Id, Rev, Project, S) of
        {ok, deleted} -> 
            {true, R1, S};
        {error, conflict} ->
            Msg = <<"This document has been updated or deleted by another user.">>,
            Msg1 = jsn:encode([{<<"message">>, Msg}]),
            cowboy_req:reply(409, [], Msg1, R1)
    end.

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
            cowboy_req:reply(409, [], Msg1, R1)
    end.
    
-spec doctype(req_data()) -> string().
doctype(R) ->
    {DoctypeBin, R1} = cowboy_req:binding(doctype, R),
    {list_or_undefined(DoctypeBin), R1}.

-spec doctype_data(req_data(), req_state()) -> req_retval().
doctype_data(R, S) ->
    {[Project, Doctype], R1} = g([project, doctype], R),
    {get(Doctype, Project, S), R1}.

-spec exists(string(), req_data(), req_state()) -> {boolean(), req_data()}.
exists(Id, R, S) ->
    {Project, R1} = project(R),
    {couch:exists(Id, Project, S), R1}.

-spec exists_id(req_data(), req_state()) -> {boolean(), req_data(), req_state()}.
exists_id(R, S) ->
    {[Project, Id], R1} = g([project, id], R),
    case Id of
        undefined -> {false, R1, S};
        _ -> {couch:exists(Id, Project, S), R1, S}
    end.
    

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

-spec field_data(req_data(), req_state()) -> req_retval().
field_data(R, S) ->
    {[Project, Field], R1} = g([project, field], R),
    {get(Field, Project, S), R1}.

-spec fieldset(req_data()) -> {string(), req_data()}.
fieldset(R) ->
    {FieldsetBin, R1} = cowboy_req:binding(fieldset, R),
    {list_or_undefined(FieldsetBin), R1}.

-spec fieldset_data(req_data(), req_state()) -> req_retval().
fieldset_data(R, S) ->
    {[Project, Fieldset], R1} = g([project, fieldset], R),
    {get(Fieldset, Project, S), R1}.

-spec g([atom()], req_data()) -> {[string() | undefined], req_data()}.
g(Keys, R) ->
  F = fun (X, {RX, Acc}) ->
    {Val, RY} = ?MODULE:X(RX),
    {RY, [Val|Acc]}
  end,
  {R1, Vals} = lists:foldl(F, {R, []}, Keys),
  {lists:reverse(Vals), R1}.
  
-spec get(string(), string(), req_state()) -> couch_ret().
get(Id, Project, S) ->
    couch:get(Id, Project, S).

-spec get(string(), string(), string(), req_state()) -> couch_ret().
get(Id, Rev, Project, S) ->
    couch:get(Id, Rev, Project, S).

-spec get_attachment(string(), string(), string(), req_state()) -> couch_ret().
get_attachment(Id, Name, Project, S) ->
    couch:get_attachment(Id, Name, Project, S).

-spec id(req_data()) -> string().
id(R) ->
    {IdBin, R1} = cowboy_req:binding(id, R),
    {list_or_undefined(IdBin), R1}.

-spec id_data(req_data(), req_state()) -> req_retval().
id_data(R, S) ->
    {[Project, Id], R1} = g([project, id], R),
    {get(Id, Project, S), R1}.

-spec id_html(atom(), req_data(), req_state()) -> {iolist(), req_data(), req_state()}.
id_html(Template, R, S) ->
    {{ok, Json}, R1} = h:id_data(R, S), 
    {ok, Html} = render:render(Template, Json),
    {Html, R1, S}.

-spec index(req_data()) -> {string() | undefined, req_data()}.
index(R) ->
    {IndexBin, R1} = cowboy_req:qs_val(<<"index">>, R),
    {list_or_undefined(IndexBin), R1}.

-spec list_or_undefined(binary()|any()) -> string()|undefined.
list_or_undefined(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
list_or_undefined(_) ->
    undefined.

-spec path_exists(req_data(), req_state()) -> {boolean(), req_data(), req_state()}.
path_exists(R, S) ->
    {Path, R1} = cowboy_req:path_info(R),
    path_exists(Path, R1, S).

-spec path_exists([string()], req_data(), req_state()) -> {boolean(), req_data(), req_state()}.
path_exists(Path, R, S) ->
    {Project, R1} = project(R),
    {ok, Json} = q:full_path([{<<"key">>, Path}], Project, S),
    Total = proplists:get_value(<<"total_rows">>, Json),
    {Total > 0, R1, S}.

-spec project(req_data()) -> {string(), req_data()}.
project(R) ->
    {ProjectBin, R1} = cowboy_req:binding(project, R),
    {list_or_undefined(ProjectBin), R1}.

-spec project_data(req_data(), req_state()) -> req_retval().
project_data(R, S) ->
    {Project, R1} = project(R),
    {get(Project -- "project-", "shimi_ima", S), R1}.

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

-spec rev_data(req_data(), req_state()) -> req_retval().
rev_data(R, S) ->
    {[Project, Rev, Id], R1} = g([project, rev, id], R),
    {get(Id, Rev, Project, S), R1}.

-spec update(jsn:json_term(), req_data(), req_state()) -> {true, req_data(), req_state()} | {ok, req_data()}.
update(Json, R, S) ->
    {[Project, Rev, Id], R1} = g([project, rev, id], R),
    Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
    Json2 = jsn:set_value(<<"_rev">>, list_to_binary(Rev), Json1),
    case couch:update(Id, Json2, Project, S) of
        {ok, updated} -> 
            {true, R1, S};
        {forbidden, Message} ->
            cowboy_req:reply(403, [], Message, R1);
        {error, conflict} ->
            Msg = <<"This document has been updated or deleted by another user.">>,
            Msg1 = jsn:encode([{<<"message">>, Msg}]),
            cowboy_req:reply(409, [], Msg1, R1)
    end.

-spec update_doctype_version(req_data(), req_state()) -> req_retval().
update_doctype_version(R, S) ->
    {[Project, Doctype], R1} = g([project, doctype], R),
    {ok, Json} = get(Doctype, Project, S),
    Id = binary_to_list(jsn:get_value(<<"_id">>, Json)),
    {ok, updated} = couch:update(Id, Json, Project, S),
    {{ok, updated}, R1}.
