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
%%% @doc Index resource.

-module(index_resource).
-author('Noah Diewald <noah@diewald.me>').

-export([init/3]).
-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2,
         from_json/2,
         is_authorized/2,
         resource_exists/2,
         rest_init/2,
         to_html/2,
         to_json/2
        ]).
-export([
         validate_authentication/3
        ]).

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

resource_exists(R, S) ->
    case proplists:get_value(target, S) of
        identifier -> h:exists_id(R, S);
        view -> h:exists_id(R, S);
        _ -> h:exists_unless_post(R, S)
    end. 

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    case proplists:get_value(target, S) of
        index -> {[<<"HEAD">>, <<"GET">>, <<"POST">>], R, S};
        view -> {[<<"HEAD">>, <<"GET">>], R, S};
        condition -> {[<<"HEAD">>, <<"GET">>], R, S};
        identifier -> {[<<"HEAD">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], R, S}
    end.
  
content_types_accepted(R, S) ->
    h:accept_json(R, S).

content_types_provided(R, S) ->
    {[{{<<"text">>, <<"html">>, []}, to_html}, {{<<"application">>, <<"json">>, []}, to_json}], R, S}.
  
delete_resource(R, S) ->
    h:delete(R, S).

to_json(R, S) ->
    case proplists:get_value(target, S) of
        index -> json_index(R, S);
        _ -> to_html(R, S)
    end.

%TODO: see document_resource about html_view  
to_html(R, S) ->
    case proplists:get_value(target, S) of
        view -> html_view(R, S);
        condition -> html_condition(R, S);
        identifier -> html_identifier(R, S)
    end.
  
from_json(R, S) ->
    case proplists:get_value(target, S) of
        index -> json_create(R, S);
        identifier -> json_update(R, S)
    end.
  
% Helpers
  
json_create(R, S) ->
    {R1, S1} = h:extract_create_data(R, S),
    i:create(R1, S1).
  
json_update(R, S) ->
    i:update(R, S).
  
json_index(R, S) ->
    {{ok, Json}, R1} = q:indexes_options(R, S),
    {jsn:encode(Json), R1, S}.

html_identifier(R, S) ->
    {{ok, Json}, R1} = h:id_data(R, S),
    Conditions = jsn:get_value(<<"conditions">>, Json),
    Fields = iolist_to_binary(jsn:encode(jsn:get_value(<<"fields">>, Json))),
    Json1 = jsn:set_value(<<"fields">>, Fields, Json),
    Labels = jsn:get_value(<<"fields_label">>, Json1),
    Json2 = jsn:set_value(<<"fields_label">>, iolist_to_binary(jsn:encode(Labels)), Json1),
    F = fun(X, {RX, Acc}) -> 
                {Html, RY} = render_conditions(X, RX, S),
                {RY, [Html|Acc]}
        end,
    {R2, RenderedConditions} = lists:foldl(F, {R1, []}, Conditions),
  
    Vals = [
            {<<"rendered_conditions">>, lists:reverse(RenderedConditions)},
            {<<"label">>, jsn:get_value(<<"fields_label">>, Json)}
            |Json2],
  
    {ok, Html} = render:render(index_edit_dtl, Vals),
    {Html, R2, S}.

html_condition(R, S) ->
    {Vals, R1} = cowboy_req:qs_vals(R),
    {Html, R2} = render_conditions(Vals, R1, S),
    {Html, R2, S}.
  
html_view(R, S) ->
    i:view(R, S).
    
validate_authentication(Props, R, S) ->
    {{ok, ProjectData}, R1} = h:project_data(R, S),
    Name = jsn:get_value(<<"name">>, ProjectData),
    ValidRoles = [<<"_admin">>, <<"manager">>, Name],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R1, S};
        false -> {proplists:get_value(auth_head, S), R1, S}
    end.

render_conditions(Arg, R, S) ->
    {{ok, Html}, R1} = 
        case is_true(jsn:get_value(<<"is_or">>, Arg)) of
            true -> 
                {index_condition_dtl:render([{<<"is_or">>, true}]), R};
            _ ->
                Negate = jsn:get_value(<<"negate">>, Arg),
                {Vals, R0} = 
                    case to_binary(jsn:get_value(<<"parens">>, Arg)) of
                        <<"open">> -> 
                            {[{<<"is_or">>, false},
                              {<<"parens">>, <<"open">>}], R};
                        <<"close">> -> 
                            {[{<<"is_or">>, false},
                              {<<"parens">>, <<"close">>}], R};
                        <<"exopen">> -> 
                            {[{<<"is_or">>, false},
                              {<<"parens">>, <<"exopen">>}], R};
                        <<"exclose">> -> 
                            {[{<<"is_or">>, false},
                              {<<"parens">>, <<"exclose">>}], R};
                        _ -> 
                            {FSLabel, R2} = get_label(jsn:get_value(<<"fieldset">>, Arg), R, S),
                            {FLabel, R3} = get_label(jsn:get_value(<<"field">>, Arg), R2, S),
                            {[{<<"is_or">>, false},
                              {<<"negate">>, (Negate =:= true) or (Negate =:= <<"true">>)},
                              {<<"fieldset">>, jsn:get_value(<<"fieldset">>, Arg)},
                              {<<"field">>, jsn:get_value(<<"field">>, Arg)},
                              {<<"operator">>, jsn:get_value(<<"operator">>, Arg)},
                              {<<"argument">>, jsn:get_value(<<"argument">>, Arg)},
                              {<<"fieldset_label">>, FSLabel},
                              {<<"field_label">>, FLabel}], R3}
                    end,
                {index_condition_dtl:render(Vals), R0}
        end,
    {Html, R1}.

get_label(<<"metadata">>, R, _S) ->
    {<<"Metadata">>, R};
get_label(Id, R, S) ->
    case field:is_meta(Id) of
        false ->
            {Project, R1} = h:project(R),
            {ok, J} = h:get(Id, Project, S),
            {jsn:get_value(<<"label">>, J), R1};
        _ ->
            {jsn:get_value(<<"label">>, field:meta_field(Id)), R}
    end.

is_true(<<"true">>) ->
    true;
is_true(true) ->
    true;
is_true(_) ->
    false.

to_binary(List) when is_list(List) ->
    list_to_binary(List);
to_binary(Binary) when is_binary(Binary) ->
    Binary;
to_binary(Other) ->
    Other.


