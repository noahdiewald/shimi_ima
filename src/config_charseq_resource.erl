%%% Copyright 2012 University of Wisconsin Madison Board of Regents.
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
%%% @doc Charseq configuration resource

-module(config_charseq_resource).
-author('Noah Diewald <noah@diewald.me>').

-export([init/3]).
-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2,
         is_authorized/2,
         resource_exists/2,
         rest_init/2
        ]).
-export([
         from_json/2,
         to_json/2,
         validate_authentication/3
        ]).

-include_lib("types.hrl").

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

resource_exists(R, S) ->
    case proplists:get_value(target, S) of
        identifier -> h:exists_id(R, S);
        _ -> h:exists_unless_post(R, S)
    end.

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    case proplists:get_value(target, S) of
        index -> {[<<"HEAD">>, <<"GET">>, <<"POST">>], R, S};
        identifier -> {[<<"HEAD">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], R, S}
    end.
  
content_types_accepted(R, S) ->
    h:accept_json(R, S).

content_types_provided(R, S) ->
    case proplists:get_value(target, S) of
        index -> {[{{<<"application">>, <<"json">>, []}, to_json}], R, S};
        identifier -> {[{{<<"application">>, <<"json">>, []}, to_json}], R, S}
    end.
  
delete_resource(R, S) ->
    h:delete(R, S).

to_json(R, S) ->
    case proplists:get_value(target, S) of
        index -> json_index(R, S);
        identifier -> json_charseq(R, S)
    end.

from_json(R, S) ->
  case proplists:get_value(target, S) of
      index -> json_create(R, S);
      identifier -> json_update(R, S)
  end.

json_charseq(R, S) ->
    {{ok, Json}, R1} = h:id_data(R, S),
    {jsn:encode(Json), R1, S}.

json_create(R, S) ->
    {R1, S1} = h:extract_create_data(R, S),
    h:create(proplists:get_value(posted_json, S1), R1, S1).

json_index(R, S) ->
    i:view(charseqs, R, S).

json_update(R, S) ->
    {ok, Body, R1} = cowboy_req:body(R),
    Json = jsn:decode(Body),
    h:update(Json, R1, S).  

% Helpers

validate_authentication(Props, R, S) ->
    ValidRoles = [<<"_admin">>, <<"manager">>],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R, S};
        false -> {proplists:get_value(auth_head, S), R, S}
    end.
