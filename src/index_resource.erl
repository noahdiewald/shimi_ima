%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of dictionary_maker.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
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
%%% @doc Index resource.

-module(index_resource).

% Webmachine API
-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         create_path/2,
         delete_resource/2,
         from_json/2,
         init/1, 
         is_authorized/2,
         post_is_create/2,
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
    Id = wrq:path_info(id, R),
  
    case proplists:get_value(target, S) of
        identifier -> {couch:exists(Id, R, S), R, S};
        view -> {couch:exists(Id, R, S), R, S};
        _ -> {true, R, S}
    end. 

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
    case proplists:get_value(target, S) of
        index -> {['HEAD', 'GET', 'POST'], R, S};
        view -> {['HEAD', 'GET'], R, S};
        condition -> {['HEAD', 'GET'], R, S};
        identifier -> {['HEAD', 'GET', 'PUT', 'DELETE'], R, S}
    end.
  
delete_resource(R, S) ->
    Msg = <<"This index has been edited or deleted by another user.">>,
    case couch:delete(R, S) of
        {ok, deleted} -> {true, R, S};
        {409, _} ->
            Message = jsn:encode([{<<"message">>, Msg}]),
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 409}, R1, S}
    end.
  
post_is_create(R, S) ->
    {true, R, S}.

create_path(R, S) ->
    Json = jsn:decode(wrq:req_body(R)),
  
    Id = utils:uuid(),
    Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
  
    Location = "http://" ++ wrq:get_req_header("host", R) ++ "/" ++ 
        wrq:path(R) ++ "/" ++ Id,
    R1 = wrq:set_resp_header("Location", Location, R),
  
    {Id, R1, [{posted_json, Json1}|S]}.

content_types_provided(R, S) ->
    {[{"text/html", to_html}], R, S}.
  
content_types_accepted(R, S) ->
    {[{"application/json", from_json}], R, S}.
  
to_html(R, S) ->
    case proplists:get_value(target, S) of
        view -> {html_view(R, S), R, S};
        index -> {html_index(R, S), R, S};
        condition -> {html_condition(R, S), R, S};
        identifier -> {html_identifier(R, S), R, S}
    end.
  
from_json(R, S) ->
    case proplists:get_value(target, S) of
        index -> json_create(R, S);
        identifier -> json_update(R, S)
    end.
  
% Helpers
  
json_create(R, S) ->
    Json = proplists:get_value(posted_json, S),
    case couch:create(doc, jsn:encode(Json), R, S) of
        {ok, created} -> {true, R, S};
        {403, Message} ->
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 403}, R1, S}
    end.
  
json_update(R, S) ->
    Json = jsn:decode(wrq:req_body(R)),
    Id = wrq:path_info(id, R),
    Rev = wrq:get_qs_value("rev", R),
    Json1 = jsn:set_value(<<"_id">>, list_to_binary(Id), Json),
    Json2 = jsn:set_value(<<"_rev">>, list_to_binary(Rev), Json1),
    Msg = <<"This index has been edited or deleted by another user.">>,

    case couch:update(doc, Id, jsn:encode(Json2), R, S) of
        {ok, updated} -> 
            {ok, _} = update_design(Json2, R, S),
            {true, R, S};
        {403, Message} ->
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 403}, R1, S};
        {409, _} ->
            Message = jsn:encode([{<<"message">>, Msg}]),
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 409}, R1, S}
    end.

update_design(Json, R, S) ->
    % Translate the conditions to javascript
    Expression = conditions:trans(jsn:get_value(<<"conditions">>, Json)),
    {ok, Design} = 
        design_index_json_dtl:render(
          [{<<"expression">>, Expression}|Json]),
    Id = "_design/" ++ wrq:path_info(id, R),
  
    case couch:exists(Id, R, S) of
        false ->
            couch:create(design, Design, R, S);
        _ ->
            couch:update(design, Design, R, S)
    end.
  
html_index(R, S) ->
    case couch:exists("_design/indexes", R, S) of
        false -> 
            {ok, VJson} = design_indexes_json_dtl:render(),
            {ok, created} = couch:create(design, VJson, R, S);
        _ -> undefined
    end,
    
    {ok, Json} = couch:get_view_json("indexes", "options", R, S),

    case wrq:get_qs_value("as", R) of
        "options" -> 
            {ok, Html} = options_dtl:render(Json);
        _Else ->
            {ok, Html} = index_index_dtl:render(Json)
    end,
    Html.

html_identifier(R, S) ->
    Json = couch:get_json(id, R, S),
    Conditions = jsn:get_value(<<"conditions">>, Json),

    Json1 = jsn:set_value(<<"fields">>,
                          iolist_to_binary(
                            jsn:encode(jsn:get_value(<<"fields">>, Json))),
                          Json),
    Labels = jsn:get_value(<<"fields_label">>, Json1),
    Json2 = jsn:set_value(<<"fields_label">>, 
                          iolist_to_binary(jsn:encode(Labels)),
                          Json1),

    F = fun(X) -> 
                render_conditions(jsn, get_value, X, R, S)
        end,
  
    RenderedConditions = lists:map(F, Conditions),
  
    Vals = [
            {<<"rendered_conditions">>, RenderedConditions},
            {<<"label">>, jsn:get_value(<<"fields_label">>, Json)}
            |Json2],
  
    {ok, Html} = index_edit_dtl:render(Vals),
    Html.

html_condition(R, S) ->
    render_conditions(wrq, get_qs_value, R, R, S).
  
html_view(R, S) ->
    IndexId = wrq:path_info(id, R),
    Limit = wrq:get_qs_value("limit", R),
    {ok, Json} = utils:get_index(IndexId, R, S),
    Index = utils:add_encoded_keys(Json),
    Vals = [{<<"limit">>, Limit}|Index],
    {ok, Html} = index_view_dtl:render(Vals),
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

% This is passed the a getter function depending on the origin of the
% request. It may have been a JSON post or put but could also be an
% URL encoded query string.
render_conditions(Module, Function, Arg, R, S) ->
    {ok, Html} = 
        case is_true(Module:Function("is_or", Arg)) of
            true -> 
                index_condition_dtl:render([{<<"is_or">>, true}]);
            _ ->
                Negate = Module:Function("negate", Arg),
                Vals = 
                    case to_binary(Module:Function("parens", Arg)) of
                        <<"open">> -> [{<<"is_or">>, false},
                                       {<<"parens">>, <<"open">>}];
                        <<"close">> -> [{<<"is_or">>, false},
                                        {<<"parens">>, <<"close">>}];
                        <<"exopen">> -> [{<<"is_or">>, false},
                                         {<<"parens">>, <<"exopen">>}];
                        <<"exclose">> -> [{<<"is_or">>, false},
                                          {<<"parens">>, <<"exclose">>}];
                        _ -> [{<<"is_or">>, false},
                              {<<"negate">>, 
                               (Negate =:= true) or (Negate =:= "true")},
                              {<<"fieldset">>, 
                               Module:Function("fieldset", Arg)},
                              {<<"field">>, Module:Function("field", Arg)},
                              {<<"operator">>, 
                               Module:Function("operator", Arg)},
                              {<<"argument">>, 
                               Module:Function("argument", Arg)},
                              {<<"fieldset_label">>, 
                               get_label(
                                 Module:Function("fieldset", Arg), R, S)},
                              {<<"field_label">>, 
                               get_label(
                                 Module:Function("field", Arg), R, S)}]
                    end,
                index_condition_dtl:render(Vals)
        end,
    Html.

get_label(<<"metadata">>, _R, _S) ->
    <<"Metadata">>;
get_label("metadata", _R, _S) ->
    <<"Metadata">>;
get_label(Id, R, S) ->
    Json = case field:is_meta(Id) of
               false ->
                   couch:get_json(Id, R, S);
               _ ->
                   field:meta_field(Id)
           end,
    jsn:get_value(<<"label">>, Json).

is_true("true") ->
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


