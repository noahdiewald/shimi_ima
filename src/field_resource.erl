%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of dictionary_maker.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% dictionary_maker is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with dictionary_maker. If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2011 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc Dictionary Maker resource for dealing with fields.

-module(field_resource).

% Webmachine API
-export([
         allowed_methods/2,
         content_types_provided/2,
         init/1, 
         is_authorized/2,
         resource_exists/2,
         to_html/2,
         to_json/2
        ]).

% Custom
-export([
         validate_authentication/3
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("config.hrl").
-include_lib("include/types.hrl").

% Standard webmachine functions

init(Opts) -> {ok, Opts}.

resource_exists(R, S) ->
    Fieldset = wrq:path_info(fieldset, R),
    Id = wrq:path_info(id, R),
  
    case proplists:get_value(target, S) of
        identifier -> identifier_exists(Id, R, S);
        index -> index_exists(Fieldset, R, S)
    end. 

identifier_exists(Id, R, S) ->
    case field:is_meta(Id) of
        true -> {true, R, [{is_meta, list_to_binary(Id)}|S]};
        _ -> {couch:exists(Id, R, S), R, S}
    end.

index_exists("metadata", R, S) -> {true, R, S};
index_exists(Fieldset, R, S) -> {couch:exists(Fieldset, R, S), R, S}.

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    {['HEAD', 'GET'], R, S}.

content_types_provided(R, S) ->
    % Currently having a problem with jquery and Accepts headers
    % this is a work around.
    case wrq:get_qs_value("format", R) of
        undefined -> {[{"text/html", to_html}, 
                       {"application/json", to_json}], R, S};
        "json" -> {[{"*/*", to_json}], R, S};
        "html" -> {[{"*/*", to_html}], R, S}
    end.

to_json(R, S) ->
    case proplists:get_value(target, S) of
        index -> {json_fields(R, S), R, S};
        identifier -> {json_field(R, S), R, S}
    end.
  
to_html(R, S) ->
    case proplists:get_value(target, S) of
        index -> {html_fields(R, S), R, S};
        identifier -> {html_field(R, S), R, S}
    end.
  
% Helpers

json_field(R, S) ->
    Field = case proplists:get_value(is_meta, S) of
                undefined ->
                    Json = couch:get_json(id, R, S),
                    Subcategory = 
                        binary_to_list(jsn:get_value(<<"subcategory">>, Json)),
                    get_allowed(Subcategory, Json, R, S);
                Id -> field:meta_field(Id)
            end,
    jsn:encode(Field).

json_fields(R, S) -> 
    Doctype = wrq:path_info(doctype, R),
    Fieldset = wrq:path_info(fieldset, R),
    {ok, Json} = q:all_fields_for_fieldset(Doctype, Fieldset, R, S),
    Rows = jsn:get_value(<<"rows">>, Json),
  
    F = fun(Row) ->
                Doc = jsn:get_value(<<"doc">>, Row),
                Subcategory = binary_to_list(
                                jsn:get_value(<<"subcategory">>, Doc)),
                get_allowed(Subcategory, Doc, R, S)
        end,
  
    jsn:encode(lists:map(F, Rows)).

html_field(R, S) -> 
    Json = couch:get_json(id, R, S),
    get_field_html(Json, R, S).
  
html_fields(R, S) -> 
    case wrq:get_qs_value("as", R) of
        undefined -> html_as_fieldset(R, S);
        "fieldset" -> html_as_fieldset(R, S);
        "options" -> html_as_options(R, S)
    end.

html_as_fieldset(R, S) -> 
    Doctype = wrq:path_info(doctype, R),
    Fieldset = wrq:path_info(fieldset, R),
    {ok, Json} = q:all_fields_for_fieldset(Doctype, Fieldset, R, S),
    Rows = jsn:get_value(<<"rows">>, Json),
  
    F = fun(Row) ->
                get_field_html(jsn:get_value(<<"doc">>, Row), R, S)
        end,
  
    lists:map(F, Rows).
  
html_as_options(R, S) ->
    Json = field:option_list(R, S),
    {ok, Html} = options_dtl:render(Json),
    Html.

get_field_html(Json, R, S) ->
    % One time use identifier
    UUID = utils:uuid(),
    Json1 = jsn:set_value(<<"instance_id">>, list_to_binary(UUID), Json),
  
    Subcategory = binary_to_list(jsn:get_value(<<"subcategory">>, Json1)),
    Template = list_to_atom("field_" ++ Subcategory ++ "_dtl"),
    {ok, Html} = Template:render(get_allowed(Subcategory, Json1, R, S)),
    Html.

get_allowed([$d, $o, $c|_], Json, R, S) -> get_allowed_docs(Json, R, S);
get_allowed("file", Json, R, S) -> get_allowed_files(Json, R, S);
get_allowed(_, Json, _, _) -> Json.

get_allowed_docs(Json, R, S) ->
    ForeignDoctype = binary_to_list(jsn:get_value(<<"source">>, Json)),
    {ok, Keys} = q:index(ForeignDoctype, R, S),
    F = fun(X) ->
                [[_|[H|_]]|_] = jsn:get_value(<<"key">>, X),
                [{<<"key">>, H}, {<<"value">>, H}]
        end,
    Allowed = lists:map(F, jsn:get_value(<<"rows">>, Keys)),
    jsn:set_value(<<"allowed">>, Allowed, Json).

get_allowed_files(Json, R, S) ->
    Path = jsn:get_value(<<"source">>, Json),
    RawAllowed = attach:get_all_full_path(Path, R, S),
    jsn:set_value(<<"allowed">>, jsn:get_value(<<"rows">>, RawAllowed), Json).
      
validate_authentication(Props, R, S) ->
    Project = couch:get_json(project, R, S),
    Name = jsn:get_value(<<"name">>, Project),
    ValidRoles = [<<"_admin">>, <<"manager">>, Name],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R, S};
        false -> {proplists:get_value(auth_head, S), R, S}
    end.
