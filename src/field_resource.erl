%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 University of Wisconsin Madison Board of Regents.
%% Copyright (c) 2010 University of Wisconsin Madison Board of Regents
%%
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
%% THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%% @doc Renders fields and fieldsets given an identifier 

-module(field_resource).

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
-include_lib("include/config.hrl").

% Standard webmachine functions

init(Opts) -> {ok, Opts}.

resource_exists(R, S) ->
  Fieldset = wrq:path_info(fieldset, R),
  Id = wrq:path_info(id, R),
  
  case proplists:get_value(target, S) of
    identifier -> {couch:exists(Id, R, S), R, S};
    index -> {couch:exists(Fieldset, R, S), R, S}
  end. 

is_authorized(R, S) ->
  proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

allowed_methods(R, S) ->
  {['HEAD', 'GET'], R, S}.

content_types_provided(R, S) ->
  {[{"text/html", to_html}], R, S}.
  
to_html(R, S) ->
  case proplists:get_value(target, S) of
    index -> {html_fields(R, S), R, S};
    identifier -> {html_field(R, S), R, S}
  end.
  
% Helpers

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
  Fieldset = wrq:path_info(fieldset, R),
  Json = couch:get_view_json(Fieldset, "fields", R, S),
  Rows = jsn:get_value(<<"rows">>, Json),
  
  F = fun(Row) ->
    get_field_html(jsn:get_value(<<"value">>, Row), R, S)
  end,
  
  lists:map(F, Rows).
  
html_as_options(R, S) ->
  Fieldset = wrq:path_info(fieldset, R),
  Json = couch:get_view_json(Fieldset, "fields_simple", R, S),
  {ok, Html} = options_dtl:render(Json),
  Html.

  
get_field_html(Json, R, S) ->
  Subcategory = binary_to_list(jsn:get_value(<<"subcategory">>, Json)),
  
  Json1 = case Subcategory of
    [$d, $o, $c|_] -> getAllowed(Json, R, S);
    _ -> Json
  end,
  
  Template = list_to_atom("field_" ++ Subcategory ++ "_dtl"),
  {ok, Html} = Template:render(Json1),
  Html.

getAllowed(Json, R, S) ->
  ForeignDoctype = binary_to_list(jsn:get_value(<<"source">>, Json)),
  RawAllowed = couch:get_view_json(ForeignDoctype, "as_key_vals", R, S),
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
