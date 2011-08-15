%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2011 University of Wisconsin Madison Board of Regents.
%% Copyright (c) 2011 University of Wisconsin Madison Board of Regents
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

-module(utils).

-export([
  clear_all/2,
  get_query/3,
  list_dir/1,
  read_file_info/1,
  report_indexing_timeout/4,
  y/1
]).

-export_type([reqdata/0]).

-include_lib("include/config.hrl").
-include_lib("include/couchdb.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-type reqdata() :: #wm_reqdata{}.

-spec list_dir(Dir :: file:name()) -> {'ok', [file:filename()]} | {'error', file:posix()}.

%% @doc Call file:list_dir and sort the results

list_dir(Dir) ->
  case file:list_dir(Dir) of
    {ok, List} -> {ok, lists:reverse(lists:sort(List))};
    {error, Reason} -> {error, Reason}
  end.

%% @doc This wrapper is required because file_lib expects that a module
%% with list_dir/1 will have read_file_info/1 in its name space.
  
read_file_info(File) ->
  file:read_file_info(File).

-spec report_indexing_timeout(Request :: fun(() -> {'ok', Something :: any()} | {'error', 'req_timedout'}), Success :: fun(({'ok', Something :: any()}) -> any()), R :: reqdata(), S :: any()) -> any() | {{'halt', 504}, R1 :: reqdata(), S:: any()}.

%% @doc Request is a fun that may return either {ok, Something} or {error,
%% req_timedout}. Success is a fun that takes the Something that may have
%% been returned by the request. If Request returns {error, req_timedout},
%% a webmachine return value that results in the server responding with
%% a 504 (gateway timeout) to the client is return. The reponse body also
%% contains a JSON error message that assumes the timeout is due to couchdb
%% being busy indexing. Otherwise, the Success fun is executed.

report_indexing_timeout(Request, Success, R, S) ->
  case Request() of
    {ok, Json} -> Success(Json);
    {error, req_timedout} -> 
      Message = jsn:encode([{<<"message">>, <<"The server is currently indexing. Try again soon.">>}]),
      R1 = wrq:set_resp_body(Message, R),
      {{halt, 504}, R1, S}
  end.

-spec y(fun()) -> any().

%% @doc A Y-combinator helper function for composing recursive funs.

y(F) ->
  G = fun (G2) -> F(fun (X) -> (G2(G2))(X) end) end,
  G(G).

-spec get_query(QueryId :: string(), R :: reqdata(), S :: any()) -> jsn:json_term().

%% @doc Take an id for a saved query and return a JSON term. This term
%% will contain either the results of querying a view or will contain only
%% [{<<"rows">>, []}] if no conditions have been defined for the query.

get_query(QueryId, R, S) ->
  case couch:get_view_json(QueryId, "index", R, S) of
    {ok, Json} -> {ok, Json};
    _ -> {ok, [{<<"rows">>, []}]}
  end.

-spec clear_all(Doctype :: string(), Project :: string()) -> ok.

%% @doc This is a helper for developers that can be used to clear previously
%% added records from the database. It doesn't pay attention to return
%% statuses.

clear_all(Doctype, Project) ->
  Url = ?ADMINDB ++ Project ++ "/_design/" ++ Doctype ++ "/_view/alldocs?limit=100",
  Header = [{"Content-Type", "application/json"}],
  DelUrl = fun (Id, Rev) -> ?ADMINDB ++ Project ++ "/" ++ Id ++ "?rev=" ++ Rev end,
  GetV = fun (K, J) -> binary_to_list(proplists:get_value(K, proplists:get_value(<<"value">>, J))) end,
  {ok, "200", _, Json} = ibrowse:send_req(Url, Header, get),
  [{_, Total},_,{<<"rows">>, Rows}] = jsn:decode(Json),
  case Total of
    Total when Total > 0 -> 
      [ibrowse:send_req(DelUrl(GetV(<<"_id">>, Row), GetV(<<"_rev">>, Row)), Header, delete)||Row <- Rows],
      clear_all(Doctype, Project);
    _ -> ok
  end.
