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
%% @doc CouchDB View API

-module(view).

-export([get_vq/1, make_vqs/1, normalize_vq/1]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/couchdb.hrl").


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
  decode_or_undefined(wrq:get_qs_value(Key, R)).


-spec getv(string(), utils:reqdata(), jsn:json_term()) -> jsn:json_term().
getv(Key, R, Default) ->
  case decode_or_undefined(wrq:get_qs_value(Key, R)) of
    undefined -> Default;
    Else -> Else
  end.


-spec decode_or_undefined(undefined | string()) -> undefined | jsn:json_term().
decode_or_undefined(undefined) ->
  undefined;
  
decode_or_undefined(Val) ->
  jsn:decode(Val).
  

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
