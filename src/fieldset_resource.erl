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
%%% @doc Renders fields and fieldsets given an identifier 

-module(fieldset_resource).

% Webmachine API
-export([
         allowed_methods/2,
         content_types_provided/2,
         init/1, 
         is_authorized/2,
         resource_exists/2,
         to_html/2
        ]).

% Custom
-export([
         validate_authentication/3
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("config.hrl").

% Standard webmachine functions

init(Opts) -> {ok, Opts}.

resource_exists(R, S) ->
    Doctype = wrq:path_info(doctype, R),
    Id = wrq:path_info(id, R),
  
    case proplists:get_value(target, S) of
        index -> {couch:exists(Doctype, R, S), R, S};
        identifier -> {couch:exists(Id, R, S), R, S}
    end. 

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    {['HEAD', 'GET'], R, S}.

content_types_provided(R, S) ->
    {[{"text/html", to_html}], R, S}.
  
to_html(R, S) ->
    case proplists:get_value(target, S) of
        identifier -> {html_fieldset(R, S), R, S};
        index -> {html_fieldsets(R, S), R, S}
    end.
  
% Helpers

html_fieldset(R, S) -> 
    Json = couch:get_json(id, R, S),
  
    Vals = [
            {<<"project_info">>, couch:get_json(project, R, S)},
            {<<"doctype_info">>, couch:get_json(doctype, R, S)}|Json
           ],
  
    {ok, Html} = fieldset_dtl:render(Vals),
    Html.
  
html_fieldsets(R, S) -> 
    Doctype = wrq:path_info(doctype, R),
    {ok, Json} = q:fieldset(Doctype, false, R, S),
    F = fun(X, Acc) ->
                [_, Id, Type, _] = jsn:get_value(<<"key">>, X),
                case Type of
                    <<"fieldset">> ->
                        [Label, Name] = jsn:get_value(<<"value">>, X),
                        [[{<<"id">>, Id},
                          {<<"key">>, Label},
                          {<<"value">>, Name}]|Acc];
                    _ ->
                        Acc
                end
        end,
    Rows = lists:foldl(F, [], jsn:get_value(<<"rows">>, Json)),
    Options = jsn:set_value(<<"rows">>, 
                            [[{<<"key">>, <<"Metadata">>},
                              {<<"value">>, <<"metadata">>},
                              {<<"id">>, <<"metadata">>}]
                             |lists:reverse(Rows)], Json),
    {ok, Html} = options_dtl:render(Options),
    Html.
    
validate_authentication(Props, R, S) ->
    Project = couch:get_json(project, R, S),
    Name = jsn:get_value(<<"name">>, Project),
    ValidRoles = [<<"_admin">>, <<"manager">>, Name],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R, S};
        false -> {proplists:get_value(auth_head, S), R, S}
    end.
