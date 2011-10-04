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
%%% @doc This module defines a charseq, more or less, information
%%% about a series of characters. This may include information about
%%% collation, the language it is used with, standard codes and
%%% linguistic information as well.

-module(charseq).
-export([
  from_json/1,
  to_json/1,
  to_renderable/1
]).

-export_type([charseq/0]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

-type regex() :: binary().

-record(charseq, {
  id :: binary(),
  rev :: binary(),
  category :: charseq,
  description :: binary(),
  characters :: [binary()],
  name :: binary(),
  sort_ignore :: [regex()],
  locale :: binary(),
  tailoring ::  binary(),
  vowels :: [binary()],
  consonants :: [binary()],
  ietf_tag :: binary(),
  iso639_tag :: binary()
}).

-type charseq() :: #charseq{}.

%% @doc Takes a json_term() decoded by jsn:decode/1 and returns either a
%% valid charseq() or an error with explanation.
 -spec from_json(Json :: jsn:json_term()) -> charseq() | {error, Reason :: string()}.
from_json(Json) ->
  #charseq{
    id = jsn:get_value(<<"_id">>, Json),
    rev = jsn:get_value(<<"_rev">>, Json),
    category = charseq,
    description = jsn:get_value(<<"description">>, Json),
    characters = jsn:get_value(<<"characters">>, Json),
    name = jsn:get_value(<<"name">>, Json),
    sort_ignore = jsn:get_value(<<"sort_ignore">>, Json),
    locale = jsn:get_value(<<"locale">>, Json),
    tailoring = jsn:get_value(<<"tailoring">>, Json),
    vowels = jsn:get_value(<<"vowels">>, Json),
    consonants = jsn:get_value(<<"consonants">>, Json),
    ietf_tag = jsn:get_value(<<"ietf_tag">>, Json),
    iso639_tag = jsn:get_value(<<"iso639_tag">>, Json)
  }.

%% @doc Takes a valid charseq() and returns a json_term() that will keep
%% some complex items as undecoded JSON for rendering.
 -spec to_renderable(charseq() | jsn:json_term()) -> [{binary(), iolist()}].
to_renderable(Charseq=#charseq{}) ->
  to_renderable(to_json(Charseq));
to_renderable(Json) ->
  Complex = [<<"characters">>, <<"sort_ignore">>, <<"vowels">>, <<"consonants">>],
  to_renderable_helper(Json, Complex).

%% @doc Takes a valid charseq() and returns a json_term().
-spec to_json(charseq()) -> jsn:json_term().
to_json(Charseq) ->
  [{<<"_id">>, Charseq#charseq.id},
   {<<"_rev">>,  Charseq#charseq.rev},
   {<<"category">>, <<"charseq">>},
   {<<"description">>, Charseq#charseq.description},
   {<<"characters">>, Charseq#charseq.characters},
   {<<"name">>, Charseq#charseq.name},
   {<<"sort_ignore">>, Charseq#charseq.sort_ignore},
   {<<"locale">>, Charseq#charseq.locale},
   {<<"tailoring">>, Charseq#charseq.tailoring},
   {<<"vowels">>, Charseq#charseq.vowels},
   {<<"consonants">>, Charseq#charseq.consonants},
   {<<"ietf_tag">>, Charseq#charseq.ietf_tag},
   {<<"iso639_tag">>, Charseq#charseq.iso639_tag}
  ].

 -spec to_renderable_helper(jsn:json_term(), [binary()]) -> jsn:json_term().
to_renderable_helper(Json, []) ->
  Json;
to_renderable_helper(Json, [H|Rest]) ->
  to_renderable_helper(jsn:set_value(H, jsn:encode(jsn:get_value(H, Json)), Json), Rest).
