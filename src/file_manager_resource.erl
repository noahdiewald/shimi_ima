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
%% @doc Resource that allows user to upload and manage files

-module(file_manager_resource).
-export([
  allowed_methods/2,
  init/1,
  is_authorized/2,
  process_post/2,
  resource_exists/2,
  to_html/2,
  validate_authentication/3
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

init(Opts) -> {ok, Opts}.

allowed_methods(R, S) ->
  case proplists:get_value(target, S) of
    upload -> {['POST'], R, S};
    _Else -> {['GET', 'HEAD'], R, S}
  end.

%% @doc Process a file upload

process_post(R, S) ->
  case attach:create(get_file_data(R, S), R, S) of
    {ok, created} -> 
      R1 = wrq:set_resp_body(<<"\"Success\"">>, R),
      {true, R1, S};
    {Code, Message} ->
      R1 = wrq:set_resp_body(Message, R),
      {{halt, Code}, R1, S}
  end.
  
to_html(R, S) ->
  case proplists:get_value(target, S) of
    main -> {html_main(R, S), R, S};
    index -> {html_index(R, S), R, S}
  end.

html_main(R, S) ->  
  User = proplists:get_value(user, S),
  Project = couch:get_json(project, R, S),
  
  Vals = [{<<"user">>, User}, {<<"project_info">>, Project}],
  
  {ok, Html} = file_manager_dtl:render(Vals),
  Html.

html_index(R, S) ->  
  Files = attach:get_all_by_path(R, S),
  
  Vals = [{<<"files">>, Files}],
  
  {ok, Html} = file_manager_listing_dtl:render(Vals),
  Html.

resource_exists(R, S) ->
  {R, S1} = attach:get_database(R, S),
  
  case proplists:get_value(target, S1) of
    identifier -> attach:file_exists(R, S1);
    path -> attach:file_path_exists(R, S1);
    _ -> attach:file_database_exists(R, S1)
  end.

is_authorized(R, S) ->
  proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

validate_authentication(Props, R, S) ->
  Project = couch:get_json(project, R, S),
  Name = jsn:get_value(<<"name">>, Project),
  ValidRoles = [<<"_admin">>, <<"manager">>, Name],
  IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
  case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
    true -> {true, R, S};
    false -> {proplists:get_value(auth_head, S), R, S}
  end.

%% @doc Get md5sum as hex list

md5sum(Bin) ->
  binary_to_hexlist(crypto:md5(Bin)).
  
%% @doc Take binary and return a hexstring

binary_to_hexlist(Bin) ->
  lists:flatten([io_lib:format("~2.16.0b",[X])||X<- binary_to_list(Bin)]).
  
get_streamed_body(done_parts, ContentType, FileName, Acc) ->
  Bin = iolist_to_binary(lists:reverse(Acc)),
  {ContentType, FileName, size(Bin)/1024.0, Bin, md5sum(Bin)};
  
get_streamed_body({{"file-chooser", {Params, Hdrs}, Content}, Next}, Types, Names, Acc) ->
  FileName = binary_to_list(proplists:get_value(<<"filename">>, Params)),
  ContentType = binary_to_list(proplists:get_value(<<"Content-Type">>, Hdrs)),
  get_streamed_body(Next(), [ContentType|Types], [FileName|Names], [Content|Acc]);
  
get_streamed_body({{"file-submit", {_Params, _Hdrs}, _Content}, Next}, Types, Names, Acc) ->
  get_streamed_body(Next(), Types, Names, Acc).

get_file_data(R, _S) ->  
  Boundary = webmachine_multipart:find_boundary(R),
  SBody = wrq:stream_req_body(R, 4096),
  SParts = webmachine_multipart:stream_parts(SBody, Boundary),
  get_streamed_body(SParts, [], [], []).
  