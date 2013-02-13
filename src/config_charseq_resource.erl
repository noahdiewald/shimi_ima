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
         create_path/2,
         delete_resource/2,
         from_json/2,
         id_html/2,
         index_html/2,
         is_authorized/2,
         post_is_create/2,
         resource_exists/2,
         rest_init/2
        ]).
-export([
         validate_authentication/3
        ]).

-include_lib("types.hrl").

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

resource_exists(R, S) ->
    case proplists:get_value(target, S) of
        identifier -> h:exists_id(R, S);
        _ -> {true, R, S}
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
        index -> {[{{<<"text">>, <<"html">>, []}, index_html}], R, S};
        identifier -> {[{{<<"text">>, <<"html">>, []}, id_html}], R, S}
    end.
  
delete_resource(R, S) ->
    h:delete(R, S).

post_is_create(R, S) ->
    {true, R, S}.

create_path(R, S) ->
    {Body, R1} = cowboy_req:body(R),
    Json = jsn:decode(Body),
    {Id, Json1} = case jsn:get_value(<<"_id">>, Json) of
                      undefined -> 
                          GenId = list_to_binary(utils:uuid()),
                          {GenId, jsn:set_value(<<"_id">>, GenId, Json)};
                      IdBin -> {IdBin, Json}
                  end,
    {Id, R1, [{posted_json, Json1}|S]}.
  
index_html(R, S) ->
    {Val, R1} = cowboy_req:qs_value(<<"as">>, R),
    case Val of
        undefined -> html_as_tabs(R1, S);
        <<"options">> -> html_as_options(R1, S)
    end.
  
html_as_options(R, S) ->
    {{ok, Json}, R1} = q:charseqs(R, S),
    {ok, Opts} = render:render(options_dtl, Json),
    {Opts, R1, S}.

html_as_tabs(R, S) ->
    {{ok, Json}, R1} = q:charseqs(R, S),
    {render:renderings(Json, config_charseq_list_elements_dtl), R1, S}.
  
id_html(R, S) ->
    {{ok, Json}, R1} = h:id_data(R, S),
    F = fun({Key, Value}) ->
                {Key, jsn:to_base64(Value)}
        end,
    Json1 = lists:map(F, Json),
    {ok, Html} = render:render(config_charseq_dtl, charseq:to_renderable(Json1)),
    {Html, R1, S}.

from_json(R, S) ->
  case proplists:get_value(target, S) of
      index -> json_create(R, S);
      identifier -> json_update(R, S)
  end.

json_create(R, S) ->  
    h:create(proplists:get_value(posted_json, S), R, S).

json_update(R, S) ->
    {Body, R1} = cowboy_req:body(R),
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

