%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of dictionary_maker.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% dictionary_maker is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with dictionary_maker. If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2012 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>

%%% @doc Resource that allows user to upload and manage files

-module(file_manager_resource).

-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2,
         from_json/2,
         get_file/2,
         init/1,
         is_authorized/2,
         process_post/2,
         resource_exists/2,
         to_html/2,
         to_json/2,
         to_null/2,
         validate_authentication/3
        ]).

-include_lib("webmachine/include/webmachine.hrl").

init(Opts) -> {ok, Opts}.

allowed_methods(R, S) ->
    case proplists:get_value(target, S) of
        upload -> {['POST'], R, S};
        identifier -> {['GET', 'HEAD', 'DELETE', 'PUT'], R, S};
        _Else -> {['GET', 'HEAD'], R, S}
    end.

resource_exists(R, S) ->
    {R, S1} = attach:get_database(R, S),
  
    case proplists:get_value(target, S1) of
        identifier -> attach:file_exists(R, S1);
        path -> attach:file_path_exists(R, S1);
        _ -> {true, R, S}
    end.

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

content_types_provided(R, S) ->
    case proplists:get_value(target, S) of
        index -> {[{"text/html", to_html}], R, S};
        identifier -> {[{"application/json", to_json}], R, S};
        list_dirs -> {[{"text/html", to_html}], R, S};
        list_files -> {[{"text/html", to_html}], R, S};
        main -> {[{"text/html", to_html}], R, S};
        path -> 
            ContentType = webmachine_util:guess_mime(wrq:disp_path(R)),
            {[{ContentType, get_file}], R, [{content_type, ContentType}|S]};
        upload -> {[{"text/plain", to_null}], R, S}
    end.
  
content_types_accepted(R, S) ->
    case proplists:get_value(target, S) of
        identifier -> {[{"application/json", from_json}], R, S};
        _Else -> {[], R, S}
    end.
  
delete_resource(R, S) ->
    Msg = <<"This document has been edited or deleted by another user.">>,
    case attach:delete(R, S) of
        {ok, deleted} -> {true, R, S};
        {409, _} ->
            Message = jsn:encode([{<<"message">>, Msg}]),
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 409}, R1, S}
    end.

from_json(R, S) ->
    Msg = <<"This document has been edited or deleted by another user.">>,
    case attach:update(R, S) of
        {ok, updated} -> {true, R, S};
        {409, _} ->
            Message = jsn:encode([{<<"message">>, Msg}]),
            R1 = wrq:set_resp_body(Message, R),
            {{halt, 409}, R1, S}
    end.

%% @doc Process a file upload
process_post(R, S) ->
    Msg = <<"File Uploaded">>,
    case attach:create(get_file_data(R, S), R, S) of
        {ok, created} -> 
            Body = [{<<"status">>, <<"success">>}, {<<"message">>, Msg}],
            R1 = wrq:set_resp_body(jsn:encode(Body), R),
            {true, R1, S};
        {Code, Message} ->
            R1 = wrq:set_resp_body(Message, R),
            {{halt, Code}, R1, S}
    end.

%% @doc A generic return value when there must be a provided content
%% type but no content is actually intended to be returned.
to_null(R, S) ->
    {<<"null">>, R, S}.
  
to_json(R, S) ->
    {jsn:encode(attach:get(R, S)), R, S}.
  
get_file(R, S) ->
    {attach:get_file(R, S), R, S}.
    
to_html(R, S) ->
    case proplists:get_value(target, S) of
        main -> {html_main(R, S), R, S};
        index -> {html_index(R, S), R, S};
        list_files -> {html_files(R, S), R, S};
        list_dirs -> {html_dirs(R, S), R, S}
    end.

html_main(R, S) ->  
    User = proplists:get_value(user, S),
    Project = wrq:path_info(project, R),
    {ok, ProjectData} = couch:get(Project -- "project-", "shimi_ima", S),
    Vals = [{<<"user">>, User}, {<<"project_info">>, ProjectData}],
    {ok, Html} = render:render(file_manager_dtl, Vals),
    Html.

html_index(R, S) ->  
    {ok, Files} = attach:get_all_by_path(R, S),
    
    Vals = [{<<"files">>, Files}],
  
    {ok, Html} = render:render(file_manager_listing_dtl, Vals),
    Html.

html_files(R, S) ->  
    Files = attach:files_by_path(R, S),
  
    Vals = [{<<"files">>, Files}],
  
    {ok, Html} = render:render(file_manager_listing_dtl, Vals),
    Html.

html_dirs(R, S) ->  
    Dirs = attach:dirs_by_path(R, S),
  
    Vals = [{<<"dirs">>, Dirs}],
  
    {ok, Html} = render:render(file_manager_paths_dtl, Vals),
    Html.

validate_authentication(Props, R, S) ->
    Project = wrq:path_info(project, R),
    {ok, ProjectData} = couch:get(Project -- "project-", "shimi_ima", S),
    Name = jsn:get_value(<<"name">>, ProjectData),
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
  
get_streamed_body({{"file-chooser", {Params, Hdrs}, Content}, Next}, Types, 
                  Names, Acc) ->
    FileName = binary_to_list(proplists:get_value(<<"filename">>, Params)),
    ContentType = binary_to_list(proplists:get_value(<<"Content-Type">>, Hdrs)),
    get_streamed_body(Next(), [ContentType|Types], [FileName|Names], 
                      [Content|Acc]);
  
get_streamed_body({{"file-submit", {_Params, _Hdrs}, _Content}, Next}, Types, 
                  Names, Acc) ->
    get_streamed_body(Next(), Types, Names, Acc).

get_file_data(R, _S) ->  
    Boundary = webmachine_multipart:find_boundary(R),
    SBody = wrq:stream_req_body(R, 4096),
    SParts = webmachine_multipart:stream_parts(SBody, Boundary),
    get_streamed_body(SParts, [], [], []).
