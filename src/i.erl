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
%%% @doc For manipulating indexes.

-module(i).
-author('Noah Diewald <noah@diewald.me>').

-export([
         create/2,
         update/2,
         view/2,
         view/3,
         view_ret/4
        ]).

-spec create(h:req_data(), h:req_state()) -> {true, h:req_data(), h:req_state()} | h:req_data().
create(R, S) ->
    Json = proplists:get_value(posted_json, S),
    {Project, R1} = h:project(R),
    case jsn:get_value(<<"category">>, Json) of
        <<"index">> ->
            h:create(Json, R1, S);
        <<"doctype">> ->
            case couch:create(Json, Project, S) of
                {ok, _} ->
                    {ok, _} = create_design(Json, Project, S),
                    {{true, proplists:get_value(create_path, S)}, R1, S};
                {forbidden, Message} ->
                    {ok, R2} = cowboy_req:reply(403, [], Message, R1),
                    {halt, R2, S}
            end
    end.

-spec create_design(jsn:json_term(), string(), h:req_state()) -> couch:ret().
create_design(DocJson, Project, S) ->
    DocId = case jsn:get_value(<<"_id">>, DocJson) of
        <<"_design/", X/binary>> -> X;
        X when is_binary(X) -> X
    end,
    Design = get_design(binary_to_list(DocId), Project, S),
    couch:create(Design, Project, [{admin, true}|S]).

%% @doc The view used here is an index such that the keys are the ids
%% of doctypes and user indexes and the values are the code for a design
%% document for those user indexes and doctypes.
-spec get_design(string(), string(), h:req_state()) -> jsn:json_term().
get_design(Id, Project, S) ->
    {ok, Json} = q:index_design(Id, Project, S),
    [Row|[]] = jsn:get_value(<<"rows">>, Json),
    jsn:decode(jsn:get_value(<<"value">>, Row)).
            
-spec update(h:req_data(), h:req_state()) -> {true, h:req_data(), h:req_state()} | h:req_data().
update(R, S) ->
    {[Id, Rev, Project], R1} = h:g([id, rev, project], R),
    {ok, Body, R2} = cowboy_req:body(R1),
    Json = jsn:decode(Body),
    Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
    Json2 = jsn:set_value(<<"_rev">>, list_to_binary(Rev), Json1),
    
    Json3 = case jsn:get_value(<<"conditions">>, Json) of
        undefined ->
            Json2;
        Conditions ->
            Expression = conditions:trans(Conditions),
            jsn:set_value(<<"expression">>, Expression, Json2)
    end,
    
    case couch:update(Id, Json3, Project, S) of
        {ok, _} -> 
            {ok, _} = update_design(Id, Project, S),
            {true, R2, S};
        {forbidden, Message} ->
            {ok, R3} = cowboy_req:reply(403, [], Message, R2),
            {halt, R3, S};
        {error, conflict} ->
            Msg = <<"This document has been updated or deleted by another user.">>,
            Msg1 = jsn:encode([{<<"message">>, Msg}]),
            {ok, R3} = cowboy_req:reply(409, [], Msg1, R2),
            {halt, R3, S}
    end.

-spec update_design(string(), string(), h:req_state()) -> {true, h:req_data(), h:req_state()} | h:req_data().
update_design(DocId, Project, S) ->
    DesignId = "_design/" ++ DocId,
    Design = get_design(DocId, Project, S),
    case h:get(DesignId, Project, S) of
        {error, not_found} ->
            create_design(Design, Project, [{admin, true}|S]);
        {ok, PrevDesign} ->
            Rev = jsn:get_value(<<"_rev">>, PrevDesign),
            Design2 = jsn:set_value(<<"_rev">>, Rev, Design),
            couch:update(DesignId, Design2, Project, [{admin, true}|S])
    end.

%% @doc Shorter form of view/3 for the case when Type is 'index'.
-spec view(h:req_data(), h:req_state()) -> {iodata(), h:req_data(), h:req_state()} | h:req_data().
view(R, S) -> view(index, R, S).

%% @doc Retrieve values from a view index. If Type is 'index' this may
%% be a user created index or a listing of documents described by
%% doctypes. The Type should correspond to a function in the q module.
-spec view(atom(), h:req_data(), h:req_state()) -> {iodata(), h:req_data(), h:req_state()} | h:req_data().
view(index, R, S) ->
    {[Id, Index, Project, Doctype], R1} = h:g([id, index, project, doctype], R),
    {QsVals, R2} = cowboy_req:qs_vals(R1),
    Ret = case {Id, Index} of
              {undefined, undefined} ->
                  Qs = view:normalize_sortkey_vq(Doctype, QsVals, Project, S),
                  q:index(Doctype, Qs, Project, S);
              {IndexId, undefined} -> 
                  Qs = view:normalize_sortkey_vq(IndexId, QsVals, Project, S),
                  q:index(IndexId, Qs, Project, S);
              {undefined, IndexId} ->
                  Qs = view:normalize_sortkey_vq(IndexId, QsVals, Project, S),
                  q:index(IndexId, Qs, Project, S)
          end,
    view_ret(index, Ret, R2, S);
view(Type, R, S) ->
    {Project, R1} = h:project(R),
    {QsVals, R2} = cowboy_req:qs_vals(R1),
    Ret = q:Type(QsVals, Project, S),
    view_ret(Type, Ret, R2, S).

%% @doc What to do with the return value from the database when
%% attempting to view an index. Handles a couple of error cases to
%% provide user feedback.
view_ret(Type, Ret, R, S) ->
    Msg = <<"still building. Please wait 5 to 10 minutes and try again.">>,
    Message = jsn:encode([{<<"message">>, Msg}, {<<"fieldname">>, Type}]),
    case Ret of
        {ok, Json} ->
            {jsn:encode(Json), R, S};
        {error, not_found} ->
            {jsn:encode([{<<"total">>, 0}, {<<"rows">>, []}]), R, S};
        {error, req_timedout} ->
            {ok, R1} = cowboy_req:reply(504, [], Message, R),
            {halt, R1, S}
    end.
