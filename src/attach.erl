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
  get_all_by_path/2,
  get_database/2,
  get_file/3,
  update/5
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

file_exists(R, S) ->
  Id = wrq:get_path_info("id", R),
  file_exists(Id, R, S).
  
file_exists(Id, R, S) ->
  DB = proplists:get_value(db, S),
  Headers = [{"Content-Type", "application/json"}],
  Url = DB ++ "/" ++ Id,
  case ibrowse:send_req(Url, Headers, head) of
    {ok, "200", _, _} -> {true, R, S};
    _ -> {false, R, S}
  end.

file_path_exists(R, S) ->
  {true, R, S}.

% Checks for database existence. If it doesn't exist try to create it.
% Will raise an exception on failure.
  
file_database_exists(R, S) ->
  DB = proplists:get_value(db, S),
  {ok, [$2,$0|[_]], _, _} = case ibrowse:send_req(DB, [], head) of
    {ok, "404", _, _} -> 
      {ok, [$2,$0|[_]], _, _} = ibrowse:send_req(DB, [], put),
      {ok, Json} = design_file_manager_json_dtl:render(),
      ibrowse:send_req(DB, [{"Content-Type", "application/json"}], post, Json);
    Else -> Else
  end,
  {true, R, S}.

get_all_by_path(R, S) ->  
  Qs = get_qs(wrq:raw_path(R)),
  BaseUrl = proplists:get_value(db, S) ++ "/",
  Path = "_design/file_manager/_view/by_path",
  FullUrl = BaseUrl ++ Path ++ Qs,
  io:format("-----(~p)--------", [FullUrl]),
  {ok, "200", _, Json} = ibrowse:send_req(FullUrl, [], get),
  jsn:decode(Json).
  
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
  
create({[ContentType], [Name], _, Content, Id}, R, S) ->
  case file_exists(Id, R, S) of
    {true, _, _} -> halt_conflict(Id, R, S);
    {false, _, _} -> create(ContentType, Name, Content, Id, R, S)
  end.

halt_conflict(Id, R, S) ->
  Json = get(Id, R, S),
  Note = ["File exists at: "|[jsn:get_value(<<"path">>, Json)]],
  Message = jsn:encode([{<<"message">>, iolist_to_binary(Note)}]),
  {409, Message}.

halt_error(Name, _R, _S) ->
  Note = ["There was a problem uploading the file: "|Name],
  Message = jsn:encode([{<<"message">>, iolist_to_binary(Note)}]),
  {500, Message}.
  
create(ContentType, Name, Content, Id, R, S) ->  
  DB = proplists:get_value(db, S),
  Url = DB ++ "/" ++ Id ++ "/" ++ mochiweb_util:quote_plus(Name),
  Headers = [{"Content-Type", ContentType}],
  case ibrowse:send_req(Url, Headers, put, Content) of
    {ok, "201", _, _} -> 
      set_initial_path(Id, Name, R, S),
      {ok, created};
    {error, retry_later} -> halt_error(Name, R, S)
  end.
  
update(Id, Rev, Json, _R, S) ->
  DB = proplists:get_value(db, S),
  Headers = [{"Content-Type", "application/json"}],
  Url = DB ++ "/" ++ Id ++ "?rev=" ++ Rev,
  {ok, [$2,$0|[_]], _, _} = ibrowse:send_req(Url, Headers, put, jsn:encode(Json)),
  {ok, updated}.

set_initial_path(Id, Name, R, S) ->
  Json = get(Id, R, S),
  Json1 = jsn:set_value(<<"path">>, list_to_binary("files/" ++ Name), Json),
  {ok, updated} = update(Id, binary_to_list(jsn:get_value(<<"_rev">>, Json1)), Json1, R, S).

get_qs([]) ->
  [];  
get_qs([$?|Rest]) ->
  [$?|Rest];
get_qs([_|Rest]) ->
  get_qs(Rest).