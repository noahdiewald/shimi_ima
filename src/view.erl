%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of dictionary_maker.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
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

-export([
         get_plus_vq/3,
         get_vq/1,
         make_vqs/1,
         normalize_plus_vq/3,
         normalize_vq/1
        ]).

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

%% @doc Takes the unique portion of a design document id and
%% webmachine state and will possibly alter the default return value
%% of get_vq/1 to set sortkeys for the startkey, if needed.
-spec get_plus_vq(string(), utils:reqdata(), [{any(),any()}]) -> view_query().
get_plus_vq(Id, R, S) ->
    Vq = get_vq(R),
    case decide_plus(Vq) of
        true ->
            set_keys_sortkeys(Id, Vq, R, S);
        false ->
            Vq
    end.

%% @doc This is like normal normalize_vq/1 except that it uses
%% get_plus_vq/3
-spec normalize_plus_vq(string(), utils:reqdata(), [{any(),any()}]) -> string().
normalize_plus_vq(Id, R, S) ->
    make_vqs(get_plus_vq(Id, R, S)).

%% @doc This takes the query string from the webmachine request state
%% and first transforms it into a view_query() using get_vq/1 before
%% transforming it into a string suitable for attaching to a URL as a
%% query string. The purpose is to clean up the user supplied values
%% and normalize them.
-spec normalize_vq(utils:reqdata()) -> string().
normalize_vq(R) ->
    make_vqs(get_vq(R)).

% Helper Functions

-spec decide_plus(view_query()) -> boolean().
decide_plus(Vq=#vq{startkey_docid=undefined}) when is_binary(Vq#vq.startkey) ->
    true;
decide_plus(_) ->
    false.

-spec set_keys_sortkeys(string(), view_query(), utils:reqdata(), [{any(),any()}]) -> view_query().
set_keys_sortkeys(Id, Vq, R, S) ->
    Json = couch:get_json(Id, R, S),
    case jsn:get_value(<<"category">>, Json) of
        <<"index">> ->
            case {jsn:get_value(<<"fields">>, Json),
                  jsn:get_value(<<"replace_pattern">>, Json)} of
                % TODO: This is here until feature is fully implemented.
                {[Field], undefined} ->
                    set_sortkey_by_field(binary_to_list(Field), Vq, R, S);
                {[Field], null} ->
                    set_sortkey_by_field(binary_to_list(Field), Vq, R, S);
                _ -> Vq
            end;
        <<"doctype">> ->
            set_sortkey_by_doctype(Id, Vq, R, S)
    end.

-spec set_sortkey_by_doctype(string(), view_query(), utils:reqdata(), [{any(),any()}]) -> view_query().
set_sortkey_by_doctype(Id, Vq, R, S) ->
    {ok, Charseqs} = couch:get_view_json(noqs, Id, "head_charseqs", R, S),
    case jsn:get_value(<<"rows">>, Charseqs) of
        [] ->
            set_sortkey_helper(<<>>, Vq, R, S);
        [First|_] ->
            Charseq = jsn:get_value(<<"value">>, First),
            set_sortkey_helper(Charseq, Vq, R, S)
    end.

-spec set_sortkey_by_field(string(), view_query(), utils:reqdata(), [{any(),any()}]) -> view_query().
set_sortkey_by_field(Field, Vq, R, S) ->
    Charseq = case field:is_meta(Field) of
                  false ->
                      Json = couch:get_json(Field, R, S),
                      jsn:get_value(<<"charseq">>, Json);
                  _ -> null
              end,
    set_sortkey_helper(Charseq, Vq, R, S).

-spec set_sortkey_helper(binary(), view_query(), utils:reqdata(), [{any(),any()}]) -> view_query().
set_sortkey_helper(Charseq, Vq, R, S) ->
    Val = Vq#vq.startkey,
    Input = [{<<"charseq">>, Charseq},{<<"value">>, Val}],
    Sortkey = charseq:get_sortkey(Input, R, S),
    Vq#vq{startkey = [[Sortkey, Val]]}.

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
