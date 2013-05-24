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
%%% @doc Dictionary Maker resource for dealing with fields.

-module(field_resource).
-author('Noah Diewald <noah@diewald.me>').

-export([init/3]).
-export([
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         resource_exists/2,
         rest_init/2,
         to_html/2,
         to_json/2
        ]).
-export([
         validate_authentication/3
        ]).

-include_lib("types.hrl").

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

resource_exists(R, S) ->
    case proplists:get_value(target, S) of
        identifier -> 
            {Id, R1} = h:id(R),
            identifier_exists(Id, R1, S);
        index ->
            {Fieldset, R1} = h:fieldset(R),
            index_exists(Fieldset, R1, S)
    end. 

identifier_exists(Id, R, S) ->
    case field:is_meta(Id) of
        true -> {true, R, [{is_meta, list_to_binary(Id)}|S]};
        _ -> h:exists(Id, R, S)
    end.

index_exists("metadata", R, S) -> {true, R, S};
index_exists(Fieldset, R, S) -> h:exists_with_deps([Fieldset, doctype], R, S).

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    {[<<"HEAD">>, <<"GET">>], R, S}.

content_types_provided(R, S) ->
    % Currently having a problem with jquery and Accepts headers
    % this is a work around. TODO: verify newer version has problem.
    {Format, R1} = cowboy_req:qs_val(<<"format">>, R),
    case Format of
        undefined -> {[{{<<"text">>, <<"html">>, []}, to_html}, 
                       {{<<"application">>, <<"json">>, []}, to_json}], R1, S};
        <<"json">> -> {[{{<<"*">>, <<"*">>, []}, to_json}], R1, S};
        <<"html">> -> {[{{<<"*">>, <<"*">>, []}, to_html}], R1, S}
    end.

to_json(R, S) ->
    case proplists:get_value(target, S) of
        index ->
            {Json, R1} = json_fields(R, S),
            {Json, R1, S};
        identifier -> 
            {Json, R1} = json_field(R, S),
            {Json, R1, S}
    end.
  
to_html(R, S) ->
    case proplists:get_value(target, S) of
        index ->
            {Html, R1} = html_fields(R, S),
            {Html, R1, S};
        identifier ->
            {Html, R1} = html_field(R, S),
            {Html, R1, S}
    end.
  
% Helpers

json_field(R, S) ->
    case proplists:get_value(is_meta, S) of
        undefined ->
            {{ok, Json}, R1} = h:id_data(R, S),
            {Project, R2} = h:project(R1),
            Subcategory = binary_to_list(jsn:get_value(<<"subcategory">>, Json)),
            Field = get_allowed(Subcategory, Json, Project, S),
            {jsn:encode(Field), R2};
        Id ->
            Field = field:meta_field(Id),
            {jsn:encode(Field), R}
    end.

json_fields(R, S) -> 
    {[Doctype, Fieldset, Project], R1} = h:g([doctype, fieldset, project], R),
    {ok, Json} = q:field(Doctype, Fieldset, Project, S),
    Rows = jsn:get_value(<<"rows">>, Json),
  
    F = fun(Row) ->
                Doc = jsn:get_value(<<"doc">>, Row),
                Subcategory = binary_to_list(jsn:get_value(<<"subcategory">>, Doc)),
                get_allowed(Subcategory, Doc, Project, S)
        end,
  
    {jsn:encode(lists:map(F, Rows)), R1}.

html_field(R, S) -> 
     {{ok, Json}, R1} = h:id_data(R, S),
     get_field_html(Json, R1, S).
  
html_fields(R, S) -> 
    {As, R1} = cowboy_req:qs_val(<<"as">>, R),
    case As of
        undefined -> html_as_fieldset(R1, S);
        <<"fieldset">> -> html_as_fieldset(R1, S);
        <<"options">> -> html_as_options(R1, S)
    end.

html_as_fieldset(R, S) -> 
    {[Doctype, Fieldset, Project], R1} = h:g([doctype, fieldset, project], R),
    {ok, Json} = q:field(Doctype, Fieldset, Project, S),
    Rows = jsn:get_value(<<"rows">>, Json),
  
    F = fun(Row, {RX, Acc}) ->
                {Field, RY} = get_field_html(jsn:get_value(<<"doc">>, Row), RX, S),
                {RY, [Field|Acc]}
        end,
  
    {R2, Fieldsets} = lists:foldl(F, {R1, []}, Rows),
    {lists:reverse(Fieldsets), R2}.
  
html_as_options(R, S) ->
    {Json, R1} = option_list(R, S),
    {ok, Html} = render:render(options_dtl, Json),
    {Html, R1}.

get_field_html(Json, R, S) ->
    {Project, R1} = h:project(R),
    % One time use identifier
    UUID = utils:uuid(),
    Json1 = field:to_json(field:from_json(Json)),
    Json2 = jsn:set_value(<<"instance_id">>, list_to_binary(UUID), Json1),
    Subcategory = binary_to_list(jsn:get_value(<<"subcategory">>, Json2)),
    Template = list_to_atom("field_" ++ Subcategory ++ "_dtl"),
    {ok, Html} = Template:render(get_allowed(Subcategory, Json2, Project, S)),
    {Html, R1}.

get_allowed([$d, $o, $c|_], Json, Project, S) -> get_allowed_docs(Json, Project, S);
get_allowed("file", Json, Project, S) -> get_allowed_files(Json, Project, S);
get_allowed(_, Json, _, _) -> Json.

get_allowed_docs(Json, Project, S) ->
    ForeignDoctype = binary_to_list(jsn:get_value(<<"source">>, Json)),
    {ok, Keys} = q:index(ForeignDoctype, [], Project, S),
    F = fun(X) ->
                H = list_to_binary(string:join([binary_to_list(Z)||[_,Z]<-jsn:get_value(<<"key">>, X), Z /= <<>>], ", ")),
                [{<<"key">>, H}, {<<"value">>, H}]
        end,
    Allowed = lists:map(F, jsn:get_value(<<"rows">>, Keys)),
    jsn:set_value(<<"allowed">>, Allowed, Json).

get_allowed_files(Json, Project, S) ->
    Path = jsn:get_value(<<"source">>, Json),
    RawAllowed = attach:get_all_full_path(Path, Project, S),
    jsn:set_value(<<"allowed">>, jsn:get_value(<<"rows">>, RawAllowed), Json).
      
validate_authentication(Props, R, S) ->
    {{ok, ProjectData}, R1} = h:project_data(R, S),
    Name = jsn:get_value(<<"name">>, ProjectData),
    ValidRoles = [<<"_admin">>, <<"manager">>, Name],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R, S};
        false -> {proplists:get_value(auth_head, S), R1, S}
    end.

option_list(R, S) ->
    F = fun(X) ->
                [Name, Label] = jsn:get_value(<<"value">>, X),
                Id = jsn:get_value(<<"id">>, X),
                [{<<"key">>, Label}, {<<"value">>, Name}, {<<"id">>, Id}]
        end,
    {[Doctype, Fieldset, Project], R1} = h:g([doctype, fieldset, project], R),
    case Fieldset of
        "metadata" -> 
            {field:meta_options(), R1};
        Fieldset -> 
            {ok, Json} = q:field(Doctype, Fieldset, false, Project, S),
            {jsn:set_value(<<"rows">>, lists:map(F, jsn:get_value(<<"rows">>, Json)), Json), R1}
    end.
