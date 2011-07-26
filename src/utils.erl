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
  list_dir/1,
  read_file_info/1,
  report_indexing_timeout/4,
  y/1
]).

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
