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
  to_json/1
]).

-export_type([charseq/0]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").

-type regex() :: string().

-record(charseq, {
  id :: string(),
  characters :: [string()],
  language_name :: string(),
  sort_ignore :: [regex()],
  locale :: string(),
  tailoring ::  string(),
  vowels :: [string()],
  consonants :: [string()],
  ietf_tag :: string(),
  iso639_tag :: string()
}).

-type charseq() :: #charseq{}.

%% @doc Takes a json_term() decoded by jsn:decode/1 and returns either a
%% valid charseq() or an error with explanation.
 -spec from_json(Json :: jsn:json_term()) -> charseq() | {error, Reason :: string()}.
from_json(_Json) ->
  undefined.

%% @doc Takes a valid charseq() and returns a json_term().
-spec to_json(charseq()) -> jsn:json_term().
to_json(_Charseq) ->
  undefined.