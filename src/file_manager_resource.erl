%%% Copyright 2012 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of Ʃimi Ima.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% dictionary_maker is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General
%%% Public License along with dictionary_maker. If not, see
%%% <http://www.gnu.org/licenses/>.

%%% @copyright 2012 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc Resource that allows user to upload and manage files

-module(file_manager_resource).
-author('Noah Diewald <noah@diewald.me>').

-export([init/3]).
-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2,
         from_json/2,
         get_file/2,
         is_authorized/2,
         process_post/2,
         resource_exists/2,
         rest_init/2,
         to_html/2,
         to_json/2,
         to_null/2
        ]).
-export([
         validate_authentication/3
        ]).

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

resource_exists(R, S) ->
    case proplists:get_value(target, S) of
        identifier -> h:exists_id(R, S);
        path -> h:path_exists(R, S);
        _ -> {true, R, S}
    end.

allowed_methods(R, S) ->
    case proplists:get_value(target, S) of
        upload -> {[<<"POST">>], R, S};
        identifier -> {[<<"HEAD">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], R, S};
        _Else -> {[<<"HEAD">>, <<"GET">>], R, S}
    end.

is_authorized(R, S) ->
    proxy_auth:is_authorized(R, [{source_mod, ?MODULE}|S]).

content_types_provided(R, S) ->
    case proplists:get_value(target, S) of
        index -> {[{{<<"text">>, <<"html">>, []}, to_html}], R, S};
        identifier -> {[{{<<"application">>, <<"json">>, []}, to_json}], R, S};
        list_dirs -> {[{{<<"text">>, <<"html">>, []}, to_html}], R, S};
        list_files -> {[{{<<"text">>, <<"html">>, []}, to_html}], R, S};
        main -> {[{{<<"text">>, <<"html">>, []}, to_html}], R, S};
        path ->
            {Filepath, R1} = cowboy_req:path(R),
            Mimetypes = [{T, get_file} || T <- mimetypes:path_to_mimes(Filepath, default)],
            {Mimetypes, R1, S};
        upload -> {[{"text/plain", to_null}], R, S}
    end.
  
content_types_accepted(R, S) ->
    case proplists:get_value(target, S) of
        identifier -> {[{{<<"application">>, <<"json">>, []}, from_json}], R, S};
        _Else -> {[], R, S}
    end.
  
delete_resource(R, S) ->
    h:delete(R, S).

from_json(R, S) ->
    {Body, R1} = cowboy_req:body(R),
    Json = jsn:decode(Body),
    h:update(Json, R1, S).

%% @doc Process a file upload
process_post(R, S) ->
    {ContentType, Path, Content, R1} = get_file_data(R, S),
    {Exists, R2, _} = h:path_exists(Path, R1, S),
    case Exists of
        true ->
            Msg = <<"File already exists. Rename or delete file and try again.">>,
            Msg1 = jsn:encode([{<<"message">>, Msg}]),
            cowboy_req:reply(409, [], Msg1, R2);
        false ->
            h:create_attachment(utils:uuid(), Path, Content, R, [{content_type, ContentType}|S])
    end.

%% @doc A generic return value when there must be a provided content
%% type but no content is actually intended to be returned.
to_null(R, S) ->
    {<<"null">>, R, S}.
  
to_json(R, S) ->
    {{ok, Json}, R1} = h:id_data(R, S),
    {jsn:encode(Json), R1, S}.
  
get_file(R, S) ->
    {Project, R1} = h:project(R),
    {Path, R2} = cowboy_req:path_info(R1),
    [Name|_] = lists:reverse(Path),
    {ok, Json} = q:full_path([{"key", Path}], Project, S),
    [Row|_] = jsn:get_value(<<"rows">>, Json),
    {ok, Body} = h:get_attachment(binary_to_list(jsn:get_value(<<"id">>, Row)), binary_to_list(Name), Project, S),
    {Body, R2, S}.
    
to_html(R, S) ->
    case proplists:get_value(target, S) of
        main -> html_main(R, S);
        index -> html_index(R, S);
        list_files -> html_files(R, S);
        list_dirs -> html_dirs(R, S)
    end.

html_main(R, S) ->  
    User = proplists:get_value(user, S),
    {{ok, ProjectData}, R1} = h:project_data(R, S),
    Vals = [{<<"user">>, User}, {<<"project_info">>, ProjectData}],
    {ok, Html} = render:render(file_manager_dtl, Vals),
    {Html, R1, S}.

html_index(R, S) ->
    {Project, R1} = h:project(R),
    {QsVals, R2} = cowboy_req:qs_values(R1),
    {ok, Files} = q:files(QsVals, Project, S),
    Vals = [{<<"files">>, Files}],
    {ok, Html} = render:render(file_manager_listing_dtl, Vals),
    {Html, R2, S}.

html_files(R, S) ->  
    {Project, R1} = h:project(R),
    {Path, R2} = cowboy_req:path_info(R1),
    {ok, Json} = q:files([{"key", Path}], Project, S),
    Vals = [{<<"files">>, Json}],
    {ok, Html} = render:render(file_manager_listing_dtl, Vals),
    {Html, R2, S}.

html_dirs(R, S) ->  
    {Project, R1} = h:project(R),
    {Path, R2} = cowboy_req:path_info(R1),
    GroupLevel = length(Path),
    StartKey = lists:reverse([0|lists:reverse(Path)]),
    EndKey = lists:reverse([[{}]|lists:reverse(Path)]),
    {ok, Dirs} = q:dirs([{"startkey", StartKey}, {"endkey", EndKey}, {"group_level", GroupLevel}], Project, S),
    Vals = [{<<"dirs">>, Dirs}],
    {ok, Html} = render:render(file_manager_paths_dtl, Vals),
    {Html, R2, S}.

validate_authentication(Props, R, S) ->
    {{ok, ProjectData}, R1} = h:project_data(R, S),
    Name = jsn:get_value(<<"name">>, ProjectData),
    ValidRoles = [<<"_admin">>, <<"manager">>, Name],
    IsMember = fun (Role) -> lists:member(Role, ValidRoles) end,
    case lists:any(IsMember, proplists:get_value(<<"roles">>, Props)) of
        true -> {true, R1, S};
        false -> {proplists:get_value(auth_head, S), R1, S}
    end.

%%% @doc Get md5sum as hex list
%md5sum(Bin) ->
%    binary_to_hexlist(crypto:md5(Bin)).
%  
%%% @doc Take binary and return a hexstring
%binary_to_hexlist(Bin) ->
%    lists:flatten([io_lib:format("~2.16.0b",[X])||X<- binary_to_list(Bin)]).
%  
%get_streamed_body(done_parts, ContentType, FileName, Acc) ->
%    Bin = iolist_to_binary(lists:reverse(Acc)),
%    {ContentType, FileName, size(Bin)/1024.0, Bin, md5sum(Bin)};
%  
%get_streamed_body({{"file-chooser", {Params, Hdrs}, Content}, Next}, Types, 
%                  Names, Acc) ->
%    FileName = binary_to_list(proplists:get_value(<<"filename">>, Params)),
%    ContentType = binary_to_list(proplists:get_value(<<"Content-Type">>, Hdrs)),
%    get_streamed_body(Next(), [ContentType|Types], [FileName|Names], 
%                      [Content|Acc]);
%  
%get_streamed_body({{"file-submit", {_Params, _Hdrs}, _Content}, Next}, Types, 
%                  Names, Acc) ->
%    get_streamed_body(Next(), Types, Names, Acc).

get_file_data(R, _S) ->
    {"", "", R}.
%    Boundary = webmachine_multipart:find_boundary(R),
%    SBody = wrq:stream_req_body(R, 4096),
%    SParts = webmachine_multipart:stream_parts(SBody, Boundary),
%    get_streamed_body(SParts, [], [], []).
