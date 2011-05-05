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
%% @doc Utilities and helpers for working with file attachments

-module(attach).

-export([
  create/3,
  file_database_exists/2,
  file_exists/2,
  file_path_exists/2,
  get/3,
  get_database/2,
  get_file/3,
  update/5
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

file_exists(R, S) ->
  {true, R, S}.

file_path_exists(R, S) ->
  {true, R, S}.

% Checks for database existence. If it doesn't exist try to create it.
% Will raise an exception on failure.
  
file_database_exists(R, S) ->
  DB = proplists:get_value(db, S),
  {ok, [$2,$0|[_]], _, _} = case ibrowse:send_req(DB, [], head) of
    {ok, "404", _, _} -> ibrowse:send_req(DB, [], put);
    Else -> Else
  end,
  {true, R, S}.
  
get_database(R, S) ->
  DB = ?ADMINDB ++ "files-" ++ lists:nthtail(8, wrq:path_info(project, R)),
  {R, [{db, DB}|S]}.

get(Id, _R, S) ->
  DB = proplists:get_value(db, S),
  Headers = [{"Content-Type", "application/json"}],
  Url = DB ++ "/" ++ Id,
  {ok, "200", _, Body} = ibrowse:send_req(Url, Headers, get),
  jsn:decode(Body).

get_file(_Id, _R, _S) ->
  undefined.
  
create({ContentType, Name, _, Content, Id}, R, S) ->
  DB = proplists:get_value(db, S),
  Url = DB ++ "/" ++ Id ++ "/" ++ Name,
  Headers = [{"Content-Type", ContentType}],
  {ok, "201", _, _} = ibrowse:send_req(Url, Headers, put, Content),
  Json = get(Id, R, S),
  Json1 = jsn:set_value(<<"path">>, list_to_binary("files/" ++ Name), Json),
  {ok, updated} = update(Id, binary_to_list(jsn:get_value(<<"_rev">>, Json1)), Json1, R, S),
  {ok, created}.
  
update(Id, Rev, Json, _R, S) ->
  DB = proplists:get_value(db, S),
  Headers = [{"Content-Type", "application/json"}],
  Url = DB ++ "/" ++ Id ++ "?rev=" ++ Rev,
  {ok, [$2,$0|[_]], _, _} = ibrowse:send_req(Url, Headers, put, jsn:encode(Json)),
  {ok, updated}.
