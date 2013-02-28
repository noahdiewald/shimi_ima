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
%%% @doc Renders fields and fieldsets given an identifier 

-module(fieldset_resource).
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

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

resource_exists(R, S) ->
    case proplists:get_value(target, S) of
        index ->
            {Doctype, R1} = h:doctype(R),
            {Exist, R2} = h:exists(Doctype, R1, S),
            {Exist, R2, S};
        identifier -> h:exists_id(R, S)
    end. 

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    {[<<"HEAD">>, <<"GET">>], R, S}.

content_types_provided(R, S) ->
    ContentTypes = case proplists:get_value(target, S) of
                       identifier -> [];
                       index -> [{{<<"application">>, <<"json">>, []}, to_json}]
                   end,
    {[{{<<"text">>, <<"html">>, []}, to_html}|ContentTypes], R, S}.
  
to_html(R, S) ->
    case proplists:get_value(target, S) of
        identifier ->
            {Html, R1} = html_fieldset(R, S),
            {Html, R1, S};
        index ->
            {Html, R1} = html_fieldsets(R, S),
            {Html, R1, S}
    end.

to_json(R, S) ->
    {[Doctype, Project], R1} = h:g([doctype, project], R),
    {ok, Json} = q:fieldset(Doctype, false, Project, S),
    Labels = field:labels(jsn:get_value(<<"rows">>, Json)),
    {jsn:encode(Labels), R1, S}.
    
% Helpers

html_fieldset(R, S) -> 
    {{ok, Json}, R1} = h:id_data(R, S),
    {Vals, R2} = h:basic_info("", "", R1, S),
    {ok, Html} = render:render(fieldset_dtl, Vals ++ Json),
    {Html, R2}.
  
html_fieldsets(R, S) -> 
    {[Doctype, Project], R1} = h:g([doctype, project], R),
    {ok, Json} = q:fieldset(Doctype, false, Project, S),
    F = fun(X, Acc) ->
                [_, Id, Type, _] = jsn:get_value(<<"key">>, X),
                case Type of
                    <<"fieldset">> ->
                        [Name, Label] = jsn:get_value(<<"value">>, X),
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
    {ok, Html} = render:render(options_dtl, Options),
    {Html, R1}.
    
validate_authentication(Props, R, S) ->
    {{ok, ProjectData}, R1} = h:project_data(R, S),
    Name = jsn:get_value(<<"name">>, ProjectData),
    ValidRoles = [<<"_admin">>, <<"manager">>, Name],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R1, S};
        false -> {proplists:get_value(auth_head, S), R1, S}
    end.
