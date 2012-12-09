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

-export([
    create/2,
    update/2
    ]).

create(R, S) ->
    Json = proplists:get_value(posted_json, S),
    case jsn:get_value(<<"category">>, Json) of
        <<"index">> ->
            h:create(Json, R, S);
        <<"doctype">> ->
            case couch:create(Json, h:project(R), S) of
                {ok, created} ->
                    {ok, _} = update_design(R, S),
                    {true, R, S};
                {forbidden, Message} ->
                    R1 = wrq:set_resp_body(Message, R),
                    {{halt, 403}, R1, S}
            end
    end.
            
update(R, S) ->
    Json = jsn:decode(wrq:req_body(R)),
    Json1 = jsn:set_value(<<"_id">>, list_to_binary(h:id(R)), Json),
    Json2 = jsn:set_value(<<"_rev">>, list_to_binary(h:rev(R)), Json1),
    
    Json3 = case jsn:get_value(<<"conditions">>, Json) of
        undefined ->
            Json2;
        Conditions ->
            Expression = conditions:trans(Conditions),
            jsn:set_value(<<"expression">>, Expression)
    end,
    
    case couch:update(h:id(R), Json3, h:project(R), S) of
        {ok, updated} -> 
            {ok, _} = update_design(R, S),
            {true, R, S};
        {forbidden, Message} ->
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 403}, R1, S};
        {error, conflict} ->
            Msg = <<"This resource has been edited or deleted by another user.">>,
            Message = jsn:encode([{<<"message">>, Msg}]),
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 409}, R1, S}
    end.

update_design(R, S) ->
    Project = h:project(R),
    Design = get_design(R, S),
    Id = "_design/" ++ h:id(R),
  
    case h:get(Id, R, S) of
        {error, not_found} ->
            couch:create(Design, Project, [{admin, true}|S]);
        {ok, PrevDesign} ->
            Rev = jsn:get_value(<<"_rev">>, PrevDesign),
            Design2 = jsn:set_value(<<"_rev">>, Rev, Design),
            couch:update(Id, Design2, Project, [{admin, true}|S])
    end.

get_design(R, S) ->
    {ok, Json} = q:index_design(R, S),
    [Row|[]] = jsn:get_value(<<"rows">>, Json),
    jsn:decode(jsn:get_value(<<"value">>, Row)).
