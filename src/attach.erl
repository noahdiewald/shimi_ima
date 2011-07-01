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
  dirs_by_path/2,
  file_database_exists/2,
  file_exists/2,
  file_path_exists/2,
  files_by_path/2,
  get/3,
  get_all_by_path/2,
  get_database/2,
  get_file/3,
  update/5
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

%% @doc Determine if the entry exists by extracting the id from the URL path

file_exists(R, S) ->
  Id = wrq:get_path_info("id", R),
  file_exists(Id, R, S).

%% @doc Determine if there is at least one entry with the filename and path
%% combination specified by the URL

file_path_exists(R, S) ->
  {true, R, S}.

%% @doc Checks for database existence. If it doesn't exist try to create it.
%% Will raise an exception on failure.
  
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

%% @doc Uses the by_path view in a generic way to get entry information. It
%% will pass on any query string given when querying the view.

get_all_by_path(R, S) ->  
  Qs = get_qs(wrq:raw_path(R)),
  BaseUrl = proplists:get_value(db, S) ++ "/",
  Path = "_design/file_manager/_view/by_path",
  FullUrl = BaseUrl ++ Path ++ Qs,
  {ok, "200", _, Json} = ibrowse:send_req(FullUrl, [], get),
  jsn:decode(Json).

%% @doc Similar to get_all_by_path/2 but only uses information available
%% in the URL path to pick out with files to list.

files_by_path(R, S) ->  
  BaseUrl = proplists:get_value(db, S) ++ "/",
  Path = "_design/file_manager/_view/by_path",
  Tokens = [list_to_binary(X) || X <- wrq:path_tokens(R)],
  [Key] = io_lib:format("~ts", [jsn:encode(Tokens)]),
  FullUrl = BaseUrl ++ Path ++ "?key=" ++ Key,
  {ok, "200", _, Json} = ibrowse:send_req(FullUrl, [], get),
  jsn:decode(Json).

dirs_by_path(R, S) ->
  % For and explanation see:
  %  http://blog.mudynamics.com/2010/10/02/using-couchdb-group_level-for-hierarchical-data/
  
  BaseUrl = proplists:get_value(db, S) ++ "/",
  Path = "_design/file_manager/_view/paths",
  Tokens = [list_to_binary(X) || X <- lists:reverse(wrq:path_tokens(R))],
  GroupLevel = integer_to_list(length(Tokens) + 1),
  
  % numbers come before strings, objects after
  
  [StartKey] = io_lib:format("~ts", [jsn:encode(lists:reverse([0|Tokens]))]),
  [EndKey] = io_lib:format("~ts", [jsn:encode(lists:reverse([[{}]|Tokens]))]),
  
  FullUrl = BaseUrl ++ Path ++ "?startkey=" ++ StartKey ++ "&endkey=" ++ EndKey ++ "&group=true&group_level=" ++ GroupLevel,
  {ok, "200", _, Json} = ibrowse:send_req(FullUrl, [], get),
  jsn:decode(Json).
    
%% @doc Returns a state with the admin URL for the file database for
%% a project
  
get_database(R, S) ->
  DB = ?ADMINDB ++ "files-" ++ lists:nthtail(8, wrq:path_info(project, R)),
  {R, [{db, DB}|S]}.

get_file(_Id, _R, _S) ->
  undefined.

%% @doc Creates an entry
  
create({[ContentType], [Name], _, Content, Id}, R, S) ->
  case file_exists(Id, R, S) of
    {true, _, _} -> halt_conflict(Id, R, S);
    {false, _, _} -> create(ContentType, Name, Content, Id, R, S)
  end.
  
%% @doc Updates an entry
  
update(Id, Rev, Json, _R, S) ->
  DB = proplists:get_value(db, S),
  Headers = [{"Content-Type", "application/json"}],
  Url = DB ++ "/" ++ Id ++ "?rev=" ++ Rev,
  {ok, [$2,$0|[_]], _, _} = ibrowse:send_req(Url, Headers, put, jsn:encode(Json)),
  {ok, updated}.
  
file_exists(Id, R, S) ->
  DB = proplists:get_value(db, S),
  Headers = [{"Content-Type", "application/json"}],
  Url = DB ++ "/" ++ Id,
  case ibrowse:send_req(Url, Headers, head) of
    {ok, "200", _, _} -> {true, R, S};
    _ -> {false, R, S}
  end.

get(Id, _R, S) ->
  DB = proplists:get_value(db, S),
  Headers = [{"Content-Type", "application/json"}],
  Url = DB ++ "/" ++ Id,
  {ok, "200", _, Body} = ibrowse:send_req(Url, Headers, get),
  jsn:decode(Body).

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
    {ok, "201", _, _} -> {ok, created};
    {error, retry_later} -> halt_error(Name, R, S)
  end.

get_qs([]) ->
  [];  
get_qs([$?|Rest]) ->
  [$?|Rest];
get_qs([_|Rest]) ->
  get_qs(Rest).