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
%%% @doc This module contains functions for manipulating documents

-module(document).

-export([
  set_sortkeys/3
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").
-include_lib("include/types.hrl").

-spec set_sortkeys(jsn:json_term(), R :: utils:reqdata(), S :: any()) -> jsn:json_term().
set_sortkeys(Doc, R, S) -> 
  jsn:set_value(<<"fieldsets">>, fieldsets_sortkeys(jsn:get_value(<<"fieldsets">>, Doc), R, S), Doc).

fieldsets_sortkeys([], _R, _S) ->
  [];
fieldsets_sortkeys(Fieldsets, R, S) ->
  fieldsets_sortkeys(Fieldsets, [], R, S).

fieldsets_sortkeys([], Acc, _R, _S) ->
  lists:reverse(Acc);
fieldsets_sortkeys([Fieldset|Rest], Acc, R, S) ->
  {FSType, Val} = case jsn:get_value(<<"multiple">>, Fieldset) of
    true ->
      {<<"multifields">>, multifields_sortkeys(jsn:get_value(<<"multifields">>, Fieldset), R, S)};
    false -> 
      {<<"fields">>, fields_sortkeys(jsn:get_value(<<"fields">>, Fieldset), R, S)}
  end,
  fieldsets_sortkeys(Rest, [jsn:set_value(FSType, Val, Fieldset)|Acc], R, S).

multifields_sortkeys([], _R, _S) ->
  [];
multifields_sortkeys(Multifields, R, S) ->
  multifields_sortkeys(Multifields, [], R, S).


multifields_sortkeys([], Acc, _R, _S) ->
  lists:reverse(Acc);
multifields_sortkeys([Mfield|Rest], Acc, R, S) ->
  multifields_sortkeys(Rest, [jsn:set_value(<<"fields">>, fields_sortkeys(jsn:get_value(<<"fields">>, Mfield), R, S), Mfield)|Acc], R, S).

fields_sortkeys([], _R, _S) ->
  [];
fields_sortkeys(Fields, R, S) ->
  fields_sortkeys(Fields, [], R, S).

fields_sortkeys([], Acc, _R, _S) ->
  lists:reverse(Acc);
fields_sortkeys([Field|Rest], Acc, R, S) ->
  fields_sortkeys(Rest, [jsn:set_value(<<"sortkey">>, get_sortkey(Field, R, S), Field)|Acc], R, S).

get_sortkey(Field, R, S) ->
  case jsn:get_value(<<"charseq">>, Field) of
    undefined -> <<>>;
    <<>> -> <<>>;
    CharseqId -> get_sortkey(CharseqId, jsn:get_value(<<"value">>, Field), R, S)
  end.

get_sortkey(_CharseqId, <<>>, _R, _S) ->
  <<>>;
get_sortkey(CharseqId, Value, R, S) when is_binary(CharseqId) ->
  Json = couch:get_json(binary_to_list(CharseqId), R, S),
  C = charseq:from_json(Json),
  case apply_patterns(C#charseq.sort_ignore, Value) of
    <<>> -> <<>>;
    Value1 -> get_sortkey(C, Value1)
  end.
  
get_sortkey(C, Value) ->
  {ok, Key} = case C#charseq.tailoring of
    <<>> -> icu:sortkey(C#charseq.locale, ustring:new(Value, utf8));
    Rules -> icu:sortkey(Rules, ustring:new(Value, utf8))
  end,
  list_to_binary(utils:binary_to_hexlist(Key)).
  
apply_patterns([], Value) ->
  Value;
apply_patterns(_, <<>>) ->
  <<>>;
apply_patterns([P|Rest], Value) ->
  apply_patterns(Rest, re:replace(Value, P, <<>>, [unicode])).
