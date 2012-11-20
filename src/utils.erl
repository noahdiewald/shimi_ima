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

%%% @copyright 2011 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc Utility functions

-module(utils).

-export([
         adb/0,
         add_charseqs_design/1,
         add_encoded_keys/1,
         binary_to_hexlist/1,
         clear_all/2,
         delete_all_design_docs/1,
         get_index/3,
         list_dir/1,
         ndb/0,
         peach/3,
         read_file_info/1,
         record_to_proplist/2,
         shuffle/1,
         update_all_by/2,
         update_all_by/3,
         uuid/0,
         y/1
        ]).

-export_type([reqdata/0]).

-include_lib("include/config.hrl").
-include_lib("include/types.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-type reqdata() :: #wm_reqdata{}.

%% @doc Return the Admin DB URL.
adb() ->
    application:get_env(admin_db).

%% @doc Return the Normal DB URL.
ndb() ->
    application:get_env(normal_db).

%% @doc Return a unique id
uuid() ->
  binary_to_hexlist(crypto:rand_bytes(16)).

%% @doc Takes a tuple that describes a view path and a function. The
%% function argument takes a document and returns {ok, document} or
%% the atom 'null'. It is applied to every document returned by the
%% view, which will be queried using the include_docs option. The
%% result of the function argument will be used to update the
%% document.
-spec update_all_by({Project :: string(), Id :: string(), View :: string()}, 
                    fun((jsn:json_term()) -> {ok, jsn:json_term()} | null)) -> 
                           ok.
update_all_by({Project, Id, View}, Fun) ->
    Url = adb() ++ Project ++ "/" ++ "_design/" ++ Id ++ "/_view/" ++ View,
    ViewData = case ibrowse:send_req(Url, [], get) of
                   {ok, "200", _, Json} -> jsn:decode(Json)
               end,
    Rows = jsn:get_value(<<"rows">>, ViewData),
    Run = fun (X) ->
                  DocId = jsn:get_value(<<"id">>, X),
                  Doc = get_doc(Project, DocId),
                  case Fun(Doc) of
                      null -> ok;
                      {ok, NewDoc} ->
                          update_doc(Project, NewDoc)
                  end
          end,
    peach(Run, Rows, 20),
    ok.

update_all_by(revs, {Project, Id, View}, Fun) ->
    Url = adb() ++ Project ++ "/" ++ "_design/" ++ Id ++ "/_view/" ++ View,
    ViewData = case ibrowse:send_req(Url, [], get) of
                   {ok, "200", _, Json} -> jsn:decode(Json)
               end,
    Rows = jsn:get_value(<<"rows">>, ViewData),
    Run = fun (X) ->
                  DocId = jsn:get_value(<<"id">>, X),
                  Doc = get_doc(revs, Project, DocId),
                  case Fun(Doc) of
                      null -> ok;
                      {ok, NewDoc} ->
                          update_doc(Project, NewDoc)
                  end
          end,
    peach(Run, Rows, 20),
    ok.
    
get_doc(Project, Doc) ->
    Url = adb() ++ Project ++ "/" ++ binary_to_list(Doc),
    {ok, "200", _, Json} = ibrowse:send_req(Url, [], get),
    jsn:decode(Json).

get_doc(revs, Project, Id) ->
    Url = adb() ++ Project ++ "/" ++ binary_to_list(Id) ++ 
        "?revs_info=true",
    {ok, "200", _, Json} = ibrowse:send_req(Url, [], get),
    Doc = jsn:decode(Json),
    jsn:set_value(
      <<"revs">>, 
      get_revs(
        Project,
        Id,
        jsn:get_value(<<"_revs_info">>, Doc), []), Doc).

get_revs(_, _, [], Acc) ->
    lists:reverse(Acc);
get_revs(_, _, [[_,{<<"status">>,<<"missing">>}]|_], Acc) ->
    lists:reverse(Acc);
get_revs(Project, Id, [[{_,Rev},_]|Rest], Acc) ->
    Url = adb() ++ Project ++ "/" ++ binary_to_list(Id) ++ 
        "?rev=" ++ binary_to_list(Rev),
    {ok, "200", _, Json} = ibrowse:send_req(Url, [], get),
    Doc = jsn:decode(Json),
    get_revs(Project, Id, Rest, [Doc|Acc]).

%% @doc A couchdb update function that doesn't require webmachine
%% info.
update_doc(Project, Doc) ->
    Url = adb() ++ Project ++ "/" ++ "/_design/doctypes/_update/stamp/" ++
        binary_to_list(jsn:get_value(<<"_id">>, Doc)),
    ibrowse:send_req(Url, [{"Content-Type", "application/json"}], put, 
                     jsn:encode(Doc)).

%% @doc Add escaped keys to view output
-spec add_encoded_keys(jsn:json_term()) -> jsn:json_term().
add_encoded_keys(Json) ->
    Rows = lists:map(fun add_encoded_key/1, jsn:get_value(<<"rows">>, Json)),
    jsn:set_value(<<"rows">>, Rows, Json).

add_encoded_key(Row) ->
    Key = jsn:get_value(<<"key">>, Row),
    jsn:set_value(<<"encoded_key">>, json_to_base64(Key), Row).

json_to_base64(Json) ->
    base64:encode(iolist_to_binary(jsn:encode(Json))).

%% @doc Call file:list_dir and sort the results
-spec list_dir(Dir :: file:name()) -> {'ok', [file:filename()]} | {'error', file:posix()}.
list_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, List} -> {ok, lists:reverse(lists:sort(List))};
        {error, Reason} -> {error, Reason}
    end.

%% @doc This wrapper is required because file_lib expects that a
%% module with list_dir/1 will have read_file_info/1 in its name
%% space.
read_file_info(File) ->
    file:read_file_info(File).

%% @doc A Y-combinator helper function for composing recursive funs.
-spec y(fun()) -> any().
y(F) ->
    G = fun (G2) -> F(fun (X) -> (G2(G2))(X) end) end,
    G(G).

%% @doc Take an id for a saved index and return a JSON term. This term
%% will contain either the results of querying a view or will contain
%% only [{<<"rows">>, []}] if no conditions have been defined for the
%% query.
-spec get_index(IndexId :: string(), R :: reqdata(), S :: any()) -> jsn:json_term().
get_index(IndexId, R, S) ->
    case q:altered_startkey(IndexId, R, S) of
        {ok, Json} -> {ok, Json};
        _ -> {ok, [{<<"rows">>, []}]}
    end.


%% @doc This is a helper for developers that can be used to clear
%% previously added records from the database. It doesn't pay
%% attention to return statuses.
-spec clear_all(Doctype :: string(), Project :: string()) -> ok.
clear_all(Doctype, Project) ->
    Url = adb() ++ Project ++ "/_design/" ++ Doctype ++ "/_view/alldocs?limit=100",
    Header = [{"Content-Type", "application/json"}],
    DelUrl = fun (Id, Rev) -> adb() ++ Project ++ "/" ++ Id ++ "?rev=" ++ Rev end,
    GetV = fun (K, J) -> binary_to_list(proplists:get_value(K, proplists:get_value(<<"value">>, J))) end,
    {ok, "200", _, Json} = ibrowse:send_req(Url, Header, get),
    [{_, Total},_,{<<"rows">>, Rows}] = jsn:decode(Json),
    case Total of
        Total when Total > 0 -> 
            [ibrowse:send_req(DelUrl(GetV(<<"_id">>, Row), GetV(<<"_rev">>, Row)), Header, delete)||Row <- Rows],
            clear_all(Doctype, Project);
        _ -> ok
    end.

%% @doc This function is to help in adding a design document needed
%% for new features that did not exist in earlier versions of the
%% software.
-spec add_charseqs_design(Project :: string()) -> 
                                 {ok, created} | {403, jsn:json_term()}.
add_charseqs_design(Project) ->
    {ok, Json} = design_charseqs_json_dtl:render(),
    Url = adb() ++ Project,
    Header = [{"Content-Type", "application/json"}],
    case ibrowse:send_req(Url,  Header, post, jsn:encode(jsn:decode(Json))) of
        {ok, "201", _, _} -> {ok, created};
        {ok, "403", _, Body} ->
            Resp = jsn:decode(Body),
            Message = jsn:get_value(<<"reason">>, Resp),
            {403, Message}
    end.

%% @doc Convert a record to a proplist, take an array of fields from
%% record_info(fields, Record) and the record itself.
-spec record_to_proplist(Fields :: [atom()], Record :: tuple()) ->
                                [{atom(), any()}].
record_to_proplist(Fields, Record) ->
    lists:zip(Fields, tl(tuple_to_list(Record))).

%% @doc Take binary and return a hexstring
-spec binary_to_hexlist(binary()) -> string().
binary_to_hexlist(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b",[X])||X<- binary_to_list(Bin)]).

%% @doc Take a function a list and a number and run the function over
%% list items in parallel. The number limits the number of concurrent
%% workers. The operation is run as a side effect, returning the atom ok.
-spec peach(F :: fun(), L :: list(), N :: integer()) -> ok.
peach(_F, [], _N) ->
    ok;
peach(F, L, N) ->
    {First, Rest} = takedrop(L, N),
    S = self(),
    Ref = erlang:make_ref(),
    Ref2 = erlang:make_ref(),
    Loop = spawn(fun() -> loop(Ref, {Ref2, S}, F, Rest) end),
    Fun = fun(I) -> spawn(fun() -> do_f({Ref, Loop}, {Ref2, S}, F, I) end) end,
    lists:foreach(Fun, First),
    counter(Ref2, length(L)).

%% @doc Randomly shuffle a list. Found on
%% http://www.trapexit.org/RandomShuffle. We associate each element in
%% the list with a random number. The list is then sorted based on the
%% generated number. We repeat this process log(n) times to ensure a
%% fair shuffle.
-spec shuffle(list()) -> list().
shuffle(List) ->
    % Determine the log n portion then randomize the list.
    randomize(round(math:log(length(List)) + 0.5), List).

-spec randomize(integer(), list()) -> list().
randomize(1, List) ->
    randomize(List);
randomize(T, List) ->
    Fun = fun(_E, Acc) ->
                  randomize(Acc)
          end,
    lists:foldl(Fun, randomize(List), lists:seq(1, (T - 1))).

-spec randomize(list()) -> list().
randomize(List) ->
    Fun = fun(A) ->
                  {random:uniform(), A}
          end,
    D = lists:map(Fun, List),
    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.
    
counter(_, 0) -> ok;
counter(Ref, N) ->
    receive
        Ref ->
            counter(Ref, N - 1)
    end.

do_f({Ref, Parent}, {Ref2, Counter}, F, I) ->
    case (catch F(I)) of
        {'EXIT', Error} ->
            error_logger:error_report(
              [{error_in_peach, {Error, {with_args, I}}}]);
        _ -> ok
    end,
    Parent ! Ref,
    Counter ! Ref2.

loop(_, _, _, []) ->
    ok;
loop(Ref, Counter, F, [Next|Rest]) ->
    S = self(),
    receive
        Ref ->
            spawn(fun() -> do_f({Ref, S}, Counter, F, Next) end),
            loop(Ref, Counter, F, Rest)
    end.

takedrop(L, N) ->
    takedrop(L, [], N).

takedrop([], Acc, _N) ->
    {lists:reverse(Acc), []};
takedrop(Rest, Acc, 0) ->
    {lists:reverse(Acc), Rest};
takedrop([H|Rest], Acc, N) ->
    takedrop(Rest, [H|Acc], N - 1).

delete_all_design_docs(DB) ->
    Url = adb() ++ DB ++ "/_all_docs?" ++ view:to_string(view:from_list([{"startkey", <<"_design/">>},{"endkey", <<"_design0">>}])),
    {ok, "200", _, Json} = ibrowse:send_req(Url, [], get),
    Designs = proplists:get_value(<<"rows">>, jsn:decode(Json)),
    F = fun(X) ->
                Id = proplists:get_value(<<"id">>, X),
                Rev = proplists:get_value(<<"rev">>, proplists:get_value(<<"value">>, X)),
                Urrl = adb() ++ DB ++ "/" ++ binary_to_list(Id) ++ "?rev=" ++ binary_to_list(Rev),
                ibrowse:send_req(Urrl, [], delete)
        end,
    lists:map(F, Designs).
