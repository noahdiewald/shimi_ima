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
%% @doc Simple proxying of basic authentication to CouchDB

-module(proxy_auth).

-export([
  is_authorized/2
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

is_authorized(ReqData, State) ->
  State1 = [{auth_head, "Basic realm=dictionary"}|State],
  case wrq:get_req_header("authorization", ReqData) of
    "Basic " ++ Base64 ->
      State2 = update_client_headers({"Authorization", "Basic " ++ Base64}, State1),
      do_basic_authentication(Base64, ReqData, State2);
    _ -> {proplists:get_value(auth_head, State1), ReqData, State1}
  end.
    
do_basic_authentication(Base64, ReqData, State) ->
  Str = base64:mime_decode_to_string(Base64),
  case string:tokens(Str, ":") of
    [Username, Password] -> couchdb_authenticate(Username, Password, ReqData, State);
    _ -> {proplists:get_value(auth_head, State), ReqData, State}
  end.
  
couchdb_authenticate(Username, Password, ReqData, State) ->
  Body = "name=" ++ Username ++ "&password=" ++ Password,
  Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
  Resp = ibrowse:send_req(?COUCHDB ++ "_session", Headers, post, Body),
  case Resp of
    {ok, "200", _, Json} -> 
      SourceMod = proplists:get_value(source_mod, State),
      SourceMod:validate_authentication(mochijson2:decode(Json), ReqData, State);
    {ok, "401", _, _} -> {proplists:get_value(auth_head, State), ReqData, State}
  end.

update_client_headers(Header, State) ->
  case proplists:get_value(headers, State) of
    undefined -> [{headers, [Header]}|State];
    Headers -> 
      State1 = proplists:delete(headers, State),
      [{headers, [Header|Headers]}|State1]
    end.
