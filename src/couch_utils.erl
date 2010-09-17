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
%% @doc Application specific CouchDB utility and helper functions

-module(couch_utils).

-export([
  get_json/3,
  get_view_json/4,
  get_uuid/2
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

get_json(project, ReqData, _State) ->
  Id = wrq:path_info(project, ReqData) -- "project-",
  Url = ?ADMINDB ++ "projects/" ++ Id,
  get_json_helper(Url, []);
  
get_json(doctype, ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  DataBaseUrl = ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
  Doctype = wrq:path_info(doctype, ReqData),
  get_json_helper(DataBaseUrl ++ Doctype, Headers);
  
get_json(id, ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  DataBaseUrl = ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
  Id = wrq:path_info(id, ReqData),
  get_json_helper(DataBaseUrl ++ Id, Headers).

get_json_helper(Url, Headers) ->  
  {ok, "200", _, JsonIn} = ibrowse:send_req(Url, Headers, get),
  mochijson2:decode(JsonIn).

get_view_json(Id, Name, ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  Url = ?COUCHDB ++ wrq:path_info(project, ReqData) ++ "/",
  {ok, "200", _, JsonIn} = ibrowse:send_req(Url ++ "_design/" ++ Id ++ "/_view/" ++ Name, Headers, get),
  mochijson2:decode(JsonIn).

get_uuid(_ReqData, State) ->
  Headers = proplists:get_value(headers, State),
  
  case ibrowse:send_req(?COUCHDB ++ "_uuids", Headers, get) of
    {ok, "200", _, Json} ->
      [Uuid] = struct:get_value(<<"uuids">>, mochijson2:decode(Json)),
      {ok, binary_to_list(Uuid)};
    _ -> undefined
  end.
