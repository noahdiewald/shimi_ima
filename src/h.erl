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

-compile(export_all).

-export_type([req_data/0, req_state/0, req_retval/0]).

-include_lib("types.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-type req_data() :: #wm_reqdata{}.
-type req_state() :: [{atom(), any()}].
-type req_retval() :: {ok, jsn:json_term() | updated | created} | {error, atom()}.

-spec basic_info(string(), string(), req_data(), req_state()) -> jsn:json_term().
basic_info(Title1, Title2, R, S) ->
    {ok, ProjectData} = project_data(R, S),
    {ok, DoctypeData} = doctype_data(R, S),
    [{<<"project_info">>, ProjectData},
     {<<"doctype_info">>, DoctypeData},
     {<<"title">>, list_to_binary(Title1 ++ doctype(R) ++ Title2)},
     {<<"user">>, proplists:get_value(user, S)}].
     
-spec charseq(req_data()) -> string().
charseq(R) ->
    wrq:path_info(charseq, R).

-spec charseq_data(req_data(), req_state()) -> req_retval().
charseq_data(R, S) ->
    get(charseq(R), project(R), S).

-spec create(jsn:json_term(), req_data(), req_state()) -> {true, req_data(), req_state()}.
create(Json, R, S) ->
    case couch:create(Json, project(R), S) of
        {ok, created} -> 
            {true, R, S};
        {forbidden, Message} ->
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 403}, R1, S}
    end.

-spec delete(req_data(), req_state()) -> {true, req_data(), req_state()} | {{halt, 409}, req_data(), req_state()}.
delete(R, S) ->
    case couch:delete(id(R), rev(R), project(R), S) of
        {ok, deleted} -> 
            {true, R, S};
        {error, conflict} ->
            Msg = <<"This document has been updated or deleted by another user.">>,
            R1 = wrq:set_resp_body(jsn:encode([{<<"message">>, Msg}]), R),
            {{halt, 409}, R1, S}
    end.

-spec delete_project(req_data(), req_state()) -> {true, req_data(), req_state()} | {{halt, 409}, req_data(), req_state()}.
delete_project(R, S) ->
    {ok, Json} = get(id(R), "shimi_ima", S),
    Rev = jsn:get_value(<<"_rev">>, Json),
    case couch:delete(id(R), binary_to_list(Rev), "shimi_ima", S) of
        {ok, deleted} ->
            {ok, deleted} = couch:rm_db("project-" ++ id(R)),
            {true, R, S};
        {error, conflict} ->
            Msg = <<"This project has been updated or deleted by another user.">>,
            R1 = wrq:set_resp_body(jsn:encode([{<<"message">>, Msg}]), R),
            {{halt, 409}, R1, S}
    end.
    
-spec doctype(req_data()) -> string().
doctype(R) ->
    wrq:path_info(doctype, R).

-spec doctype_data(req_data(), req_state()) -> req_retval().
doctype_data(R, S) ->
    get(doctype(R), project(R), S).

-spec exists(string(), req_data(), req_state()) -> boolean().
exists(Id, R, S) ->
    couch:exists(Id, project(R), S).

-spec field(req_data()) -> string().
field(R) ->
    wrq:path_info(field, R).

-spec field_data(req_data(), req_state()) -> req_retval().
field_data(R, S) ->
    get(field(R), project(R), S).

-spec fieldset(req_data()) -> string().
fieldset(R) ->
    wrq:path_info(fieldset, R).

-spec fieldset_data(req_data(), req_state()) -> req_retval().
fieldset_data(R, S) ->
    get(fieldset(R), project(R), S).

-spec get(string(), string() | req_data(), req_state()) -> req_retval().
get(Id, Project, S) when is_list(Project) ->
    couch:get(Id, Project, S);
get(Id, R, S) ->
    get(Id, project(R), S).

-spec get(string(), string(), string() | req_data(), req_state()) -> req_retval().
get(Id, Rev, Project, S) when is_list(Project) ->
    couch:get(Id, Rev, Project, S);
get(Id, Rev, R, S) ->
    get(Id, Rev, project(R), S).

-spec id(req_data()) -> string().
id(R) ->
    wrq:path_info(id, R).

-spec id_data(req_data(), req_state()) -> req_retval().
id_data(R, S) ->
    get(id(R), project(R), S).

id_html(Template, R, S) ->
    {ok, Json} = h:id_data(R, S), 
    {ok, Html} = render:render(Template, Json),
    {Html, R, S}.
    
-spec project(req_data()) -> string().
project(R) ->
    wrq:path_info(project, R).
    
-spec project_data(req_data(), req_state()) -> req_retval().
project_data(R, S) ->
    get(project(R) -- "project-", "shimi_ima", S).
    
-spec rev(req_data()) -> string().
rev(R) ->
    case wrq:path_info(rev, R) of
        undefined ->
            wrq:get_qs_value("rev", R);
         Rev ->
             Rev
     end.
    
-spec rev_data(req_data(), req_state()) -> req_retval().
rev_data(R, S) ->
    get(id(R), rev(R), project(R), S).

-spec update(jsn:json_term(), req_data(), req_state()) -> {true, req_data(), req_state()} | {{halt, integer()}, req_data(), req_state()}.
update(Json, R, S) ->
     Json1 = jsn:set_value(<<"_id">>, list_to_binary(id(R)), Json),
     Json2 = jsn:set_value(<<"_rev">>, list_to_binary(rev(R)), Json1),
     case couch:update(id(R), Json2, project(R), S) of
        {ok, updated} -> 
            {true, R, S};
        {forbidden, Message} ->
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 403}, R1, S};
        {error, conflict} ->
            Msg = <<"This resource has been updated or deleted by another user.">>,
            R1 = wrq:set_resp_body(jsn:encode([{<<"message">>, Msg}]), R),
            {{halt, 409}, R1, S}
    end.
    
-spec update_doctype_version(req_data(), req_state()) -> req_retval().
update_doctype_version(R, S) ->
    {ok, Json} = get(doctype(R), R, S),
    Id = binary_to_list(jsn:get_value(<<"_id">>, Json)),
    {ok, updated} = couch:update(Id, Json, project(R), S).
