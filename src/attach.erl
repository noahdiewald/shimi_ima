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
  delete/2,
  dirs_by_path/2,
  file_database_exists/2,
  file_exists/2,
  file_path_exists/2,
  files_by_path/2,
  get/2,
  get/3,
  get_all_full_path/3,
  get_all_by_path/2,
  get_database/2,
  get_file/2,
  update/2
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").
-include_lib("include/couchdb.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
% API Functions   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

%% @doc Determine if the entry exists by extracting the id from the URL path
file_exists(R, S) ->
  Id = wrq:path_info(id, R),
  file_exists(Id, R, S).

%% @doc Determine if there is at least one entry with the filename and path
%% combination specified by the URL
file_path_exists(R, S) ->
  Path = jsn:encode_to_list([list_to_binary(X) || X <- wrq:path_tokens(R)]),
  file_path_exists(Path, R, S). 
  
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
  Url = get_view_url("by_path", couch:get_vq(R), R, S),
  get_json(Url).

%% @doc Retrieve the path, including the file name, with an explicit startkey
get_all_full_path(Key, R, S) ->
  StartKey = jsn:encode_to_list(lists:reverse([0|lists:reverse(Key)])),
  EndKey = jsn:encode_to_list(lists:reverse([[{}]|lists:reverse(Key)])),
  Url = get_view_url("full_paths", #vq{startkey=StartKey, endkey=EndKey}, R, S),
  get_json(Url).

%% @doc Similar to get_all_by_path/2 but only uses information available
%% in the URL path to pick out with files to list.
files_by_path(R, S) ->  
  Tokens = [list_to_binary(X) || X <- wrq:path_tokens(R)],
  files_by_path(Tokens, R, S).

%% @doc Get path portions to enable a hierarchical directory browsing
%% interface
dirs_by_path(R, S) ->
  % For and explanation see:
  % http://blog.mudynamics.com/2010/10/02/using-couchdb-group_level-for-hierarchical-data/
  Tokens = [list_to_binary(X) || X <- lists:reverse(wrq:path_tokens(R))],
  GroupLevel = integer_to_list(length(Tokens) + 1),
  
  % numbers come before strings, objects after
  StartKey = jsn:encode_to_list(lists:reverse([0|Tokens])),
  EndKey = jsn:encode_to_list(lists:reverse([[{}]|Tokens])),
  
  Query = #vq{
    startkey=StartKey,
    endkey=EndKey,
    group="true",
    group_level=GroupLevel
  },
  Url = get_view_url("paths", Query, R, S),
  get_json(Url).
    
%% @doc Returns a state with the admin URL for the file database for
%% a project
get_database(R, S) ->
  DB = ?ADMINDB ++ "files-" ++ lists:nthtail(8, wrq:path_info(project, R)),
  {R, [{db, DB}|S]}.

%% @doc Retrieve a file attachment. This can be done by supplying only
%% the path or, more effieciently if there is an id specified in the
%% query string.
get_file(R, S) ->
  get_file(wrq:get_qs_value("id", R), R, S).
  
%% @doc Retrieve a file attachment by id or if the id is undefined, examine
%% the request path to find the id of the requested file document.
get_file(undefined, R, S) ->
  Path = jsn:encode_to_list([list_to_binary(X) || X <- wrq:path_tokens(R)]),
  Url = get_view_url("full_paths", #vq{key=Path}, R, S),
  Json = get_json(Url),
  [Row|_] = proplists:get_value(<<"rows">>, Json),
  get_file(binary_to_list(proplists:get_value(<<"id">>, Row)), R, S);
  
get_file(Id, R, S) ->
  [Name|_] = lists:reverse(wrq:path_tokens(R)),
  Url = get_url([Id, Name], [], R, S),
  get_body(Url).

%% @doc Creates an entry with information passed by processing a multi-part
%% post
create({[ContentType], [Name], _, Content, Id}, R, S) ->
  Path = jsn:encode_to_list([list_to_binary(Name)]),
  {{Exists, _, _}, {PathExists, _, _}} = {file_exists(Id, R, S), file_path_exists(mochiweb_util:quote_plus(Path), R, S)},
  case Exists or PathExists of
    true -> halt_conflict();
    false -> create(ContentType, Name, Content, Id, R, S)
  end.
  
%% @doc Updates an entry
update(R, S) ->
  Headers = [{"Content-Type", "application/json"}],
  Rev = wrq:get_qs_value("rev", R),
  Id = wrq:path_info(id, R),
  Url = get_url([Id], ["rev=" ++ Rev], R, S),
  Json = jsn:decode(wrq:req_body(R)),
  Path = lists:reverse(proplists:get_value(<<"path">>, Json, [])),
  [{Filename, _}|_] = proplists:get_value(<<"_attachments">>, Json, []),
  Path1 = jsn:encode_to_list(lists:reverse([Filename|Path])),
  
  case file_path_exists(mochiweb_util:quote_plus(Path1), R, S) of
    {true, R, S} -> halt_conflict();
    {false, R, S} ->
      {ok, [$2,$0|[_]], _, _} = ibrowse:send_req(Url, Headers, put, jsn:encode(Json)),
      {ok, updated}
  end.

%% @doc Delete an entry. The id should be in the request path.
delete(R, S) ->
  Id = wrq:path_info(id, R),
  Rev = wrq:get_qs_value("rev", R),
  Url = get_url([Id], ["rev=" ++ Rev], R, S),
  Headers = [{"Content-Type","application/json"}],
  case ibrowse:send_req(Url, Headers, delete) of
    {ok, "200", _, _} -> {ok, deleted};
    {ok, "409", _, _} -> {409, <<"Conflict">>}
  end.

%% @doc Get a file document, where the id is in the request path.
get(R, S) ->
  Id = wrq:path_info(id, R),
  get(Id, R, S).

%% @doc Get a file document, specifying the id explicitly
get(Id, R, S) ->
  Url = get_url([Id], [], R, S),
  get_json(Url).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
% Helper Functions   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

%% @doc Send a get request and decode JSON body
get_json(Url) -> 
  {ok, "200", _, Json} = ibrowse:send_req(Url, [], get),
  jsn:decode(Json).

%% @doc Send a get request and return body
get_body(Url) -> 
  {ok, "200", _, Body} = ibrowse:send_req(Url, [], get),
  Body.

%% @doc Build an URL to interact with files database
get_url(Path, Query, R, S) ->
  case proplists:get_value(db, S) of
    undefined ->
      {R, S1} = get_database(R, S),
      get_url(Path, Query, R, S1);
    Db ->
      string:join([Db|Path], "/") ++ "?" ++ string:join(Query, "&")
  end.

%% @doc Build an URL to query a view
get_view_url(View, Query, R, S) ->
  case proplists:get_value(db, S) of
    undefined ->
      {R, S1} = get_database(R, S),
      get_view_url(View, Query, R, S1);
    Db ->
      V = "_design/file_manager/_view",
      string:join([Db, V, View], "/") ++ "?" ++ couch:make_vqs(Query)
  end.
  
%% @doc create/3 helper
create(ContentType, Name, Content, Id, R, S) ->  
  Url = get_url([Id, mochiweb_util:quote_plus(Name)], [], R, S),
  Headers = [{"Content-Type", ContentType}],
  case ibrowse:send_req(Url, Headers, put, Content) of
    {ok, "201", _, _} -> {ok, created};
    {error, retry_later} -> halt_error(Name, R, S)
  end.
  
%% @doc file_path_exists/2 helper
file_path_exists(Path, R, S) ->
  Url = get_view_url("full_paths", #vq{key=Path}, R, S),
  {ok, "200", _, Body} = ibrowse:send_req(Url, [], get),
  Json = jsn:decode(Body),
  Total = length(proplists:get_value(<<"rows">>, Json)),
  {Total > 0, R, S}.
  
%% @doc files_by_path/2 helper
files_by_path(Tokens, R, S) ->
  Url = get_view_url("by_path", #vq{key=jsn:encode_to_list(Tokens)}, R, S),
  {ok, "200", _, Json} = ibrowse:send_req(Url, [], get),
  jsn:decode(Json).

%% @doc  file_exists/2 helper
file_exists(Id, R, S) ->
  Headers = [{"Content-Type", "application/json"}],
  Url = get_url([Id], [], R, S),
  case ibrowse:send_req(Url, Headers, head) of
    {ok, "200", _, _} -> {true, R, [{identifier, Id}|S]};
    _ -> {false, R, S}
  end.

%% @doc For sending back a conflict error and message.  
halt_conflict() ->
  Message = jsn:encode([{<<"status">>, <<"error">>}, {<<"message">>, <<"File already exists.">>}]),
  {409, Message}.

%% @doc For sending back a 500 error that might otherwise be hidden in the
%% couchdb interaction
halt_error(Name, _R, _S) ->
  Note = ["There was a problem uploading the file: "|Name],
  Message = jsn:encode([{<<"message">>, iolist_to_binary(Note)}]),
  {500, Message}.
