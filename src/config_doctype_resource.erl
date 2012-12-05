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
%%% @doc The resource used accessing and editing document types in a
%%% configuration context.

-module(config_doctype_resource).

% Webmachine API
-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         create_path/2,
         delete_resource/2,
         init/1, 
         is_authorized/2,
         post_is_create/2,
         process_post/2,
         resource_exists/2
        ]).

% Custom
-export([
         from_json/2,
         id_html/2,
         index_html/2,
         provide_null/2,
         validate_authentication/3
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/types.hrl").

% Standard webmachine functions

init(Opts) -> {ok, Opts}.

resource_exists(R, S) ->
    Id = wrq:path_info(id, R),
    case proplists:get_value(target, S) of
        identifier -> {h:exists(Id, R, S), R, S};
        index -> {true, R, S};
        touch -> {h:exists(Id, R, S), R, S}
    end.

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    case proplists:get_value(target, S) of
        index -> {['HEAD', 'GET', 'POST'], R, S};
        touch -> {['HEAD', 'POST'], R, S};
        identifier -> {['HEAD', 'GET', 'PUT', 'DELETE'], R, S}
    end.
  
delete_resource(R, S) ->
    h:delete(R, S).
  
post_is_create(R, S) ->
    case proplists:get_value(target, S) of
        touch -> {false, R, S};
        _Else -> {true, R, S}
    end.

create_path(R, S) ->
    Json = jsn:decode(wrq:req_body(R)),
    Id = binary_to_list(jsn:get_value(<<"_id">>, Json)),
    Location = "http://" ++ wrq:get_req_header("host", R) ++ "/" ++ 
        wrq:path(R) ++ "/" ++ Id,
    R1 = wrq:set_resp_header("Location", Location, R),
    {Id, R1, [{posted_json, Json}|S]}.

content_types_provided(R, S) ->
    case proplists:get_value(target, S) of
        index -> {[{"text/html", index_html}], R, S};
        touch -> {[{"*/*", index_html}], R, S};
        identifier -> {[{"text/html", id_html}], R, S}
    end.
  
content_types_accepted(R, S) ->
    {[{"application/json", from_json}], R, S}.

process_post(R, S) ->
    Doctype = wrq:path_info(id, R),
    Project = wrq:path_info(project, R),
    document_toucher:start(Project, Doctype, S),
    {{halt, 204}, R, S}.

provide_null(R, S) ->
    {[<<>>], R, S}.
    
index_html(R, S) ->
    {ok, Json} = q:doctypes(true, R, S),
    {render:renderings(Json, config_doctype_list_elements_dtl), R, S}.
  
id_html(R, S) ->
    h:id_html(config_doctype_dtl,  R, S).
  
from_json(R, S) ->
    case proplists:get_value(target, S) of
        index -> json_create(R, S);
        identifier -> json_update(R, S)
    end.

json_create(R, S) ->  
    Json = jsn:decode(wrq:req_body(R)),
    h:create(Json, R, S).

json_update(R, S) ->
    Json = jsn:decode(wrq:req_body(R)),
    h:update(Json, R, S).

% Helpers

validate_authentication(Props, R, S) ->
    ValidRoles = [<<"_admin">>, <<"manager">>],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R, S};
        false -> {proplists:get_value(auth_head, S), R, S}
    end.
