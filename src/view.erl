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
%%% @doc CouchDB View API

-module(view).

-export([get_vq/1, make_vqs/1, normalize_vq/1]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/types.hrl").


%% @doc Take a view_query record and return a URL query string

-spec make_vqs(view_query()) -> string().
make_vqs(VQ) ->
    string:join(make_vqs(VQ, []), "&").


%% @doc Process an incoming URL query string into a view_query object

-spec get_vq(utils:reqdata()) -> view_query().
get_vq(R) ->
    #vq{
        key = getv("key", R), 
        startkey = getv("startkey", R), 
        startkey_docid = wrq:get_qs_value("startkey_docid", R), 
        endkey = getv("endkey", R), 
        endkey_docid = wrq:get_qs_value("endkey_docid", R), 
        limit = getv("limit", R), 
        stale = stale_val(wrq:get_qs_value("stale", R)), 
        descending = getv("descending", R, false), 
        skip = getv("skip", R, 0), 
        group_level = getv("group_level", R, exact), 
        reduce = getv("reduce", R, true), 
        include_docs = getv("include_docs", R, false), 
        inclusive_end = getv("inclusive_end", R, true),
        update_seq = getv("inclusive_end", R, false)}.

-spec get_plus_vq(utils:reqdata()) -> view_query().
get_plus_vq(R) ->
    Vq = get_vq(R),
    set_keys_sortkeys(Vq).

-spec set_keys_sortkeys(view_query()) -> view_query().
set_keys_sortkeys(Vq) ->
    undefined.

-spec norm_plus_vq(utils:reqdata()) -> view_query().
norm_plus_vq(R) ->
    make_vqs(get_plus_vq(R)).


-spec normalize_vq(utils:reqdata()) -> view_query().
normalize_vq(R) ->
    make_vqs(get_vq(R)).

% Helper Functions

-spec stale_val(string() | undefined) -> 'ok' | 'update_after' | 'undefined'.
stale_val("ok") -> ok;

stale_val("update_after") -> update_after;

stale_val(_) -> undefined.


-spec getv(string(), utils:reqdata()) -> undefined | jsn:json_term().
getv(Key, R) ->
    getv(Key, R, undefined).


-spec getv(string(), utils:reqdata(), jsn:json_term()) -> jsn:json_term().
getv(Key, R, Default) ->
    case wrq:get_qs_value(Key, R) of
        undefined -> Default;
        Else -> jsn:decode(Else)
    end.


-spec encode(jsn:json_term()) -> string().
encode(Term) ->
    mochiweb_util:quote_plus(jsn:encode_to_list(Term)).


-spec make_vqs(view_query(), [string()]) -> [string()].
make_vqs(VQ, Acc) when VQ#vq.key /= undefined ->
    make_vqs(VQ#vq{key = undefined}, ["key=" ++ encode(VQ#vq.key)|Acc]);

make_vqs(VQ, Acc) when VQ#vq.startkey /= undefined ->
    make_vqs(VQ#vq{startkey = undefined}, ["startkey=" ++ encode(VQ#vq.startkey)|Acc]);

make_vqs(VQ, Acc) when VQ#vq.startkey_docid /= undefined ->
    make_vqs(VQ#vq{startkey_docid = undefined}, ["startkey_docid=" ++ mochiweb_util:quote_plus(VQ#vq.startkey_docid)|Acc]);

make_vqs(VQ, Acc) when VQ#vq.endkey /= undefined ->
    make_vqs(VQ#vq{endkey=undefined}, ["endkey=" ++ encode(VQ#vq.endkey)|Acc]);

make_vqs(VQ, Acc) when VQ#vq.endkey_docid /= undefined ->
    make_vqs(VQ#vq{endkey_docid=undefined}, ["endkey_docid=" ++ mochiweb_util:quote_plus(VQ#vq.endkey_docid)|Acc]);

make_vqs(VQ, Acc) when VQ#vq.limit /= undefined ->
    make_vqs(VQ#vq{limit = undefined}, ["limit=" ++ integer_to_list(VQ#vq.limit)|Acc]);

make_vqs(VQ, Acc) when VQ#vq.stale /= undefined ->
    Val = case VQ#vq.stale of
              ok -> "ok";
              update_after -> "update_after"
          end,
    make_vqs(VQ#vq{stale = undefined}, ["stale=" ++ Val|Acc]);

make_vqs(VQ, Acc) when VQ#vq.descending /= false ->
    make_vqs(VQ#vq{descending = false}, ["descending=true"|Acc]);

make_vqs(VQ, Acc) when VQ#vq.skip /= 0 ->
    make_vqs(VQ#vq{skip = 0}, ["skip=" ++ integer_to_list(VQ#vq.skip)|Acc]);

make_vqs(VQ, Acc) when VQ#vq.group_level /= exact ->
    make_vqs(VQ#vq{group_level=exact}, ["group_level=" ++ integer_to_list(VQ#vq.group_level)|Acc]);

make_vqs(VQ, Acc) when VQ#vq.reduce /= true  ->
    make_vqs(VQ#vq{reduce=true}, ["reduce=false"|Acc]);

make_vqs(VQ, Acc) when VQ#vq.include_docs /= false ->
    make_vqs(VQ#vq{include_docs=false}, ["include_docs=true"|Acc]);

make_vqs(VQ, Acc) when VQ#vq.inclusive_end /= true ->
    make_vqs(VQ#vq{inclusive_end=true}, ["inclusive_end=false"|Acc]);

make_vqs(VQ, Acc) when VQ#vq.update_seq /= false ->
    make_vqs(VQ#vq{inclusive_end=false}, ["inclusive_end=true"|Acc]);

make_vqs(#vq{
            key = undefined, 
            startkey = undefined, 
            startkey_docid = undefined, 
            endkey = undefined, 
            endkey_docid = undefined, 
            limit = undefined, 
            stale = undefined, 
            descending = false,
            skip = 0, 
            group_level = exact, 
            reduce = true, 
            include_docs = false,
            inclusive_end = true,
            update_seq = false}, Acc) -> Acc.
