%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of ucol.
%%%
%%% ucol is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% ucol is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with ucol. If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2011 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc This is a NIF library for ICU Unicode collation. For more information
%%% on some of the concepts used here, see the ICU documentation. Also,
%%% this NIF is supposed to be used together with the icu4e library, which
%%% supplies functions for creating and comparing UStrings, an ICU data type,
%%% which is basically an UTF-16 string.

-module(icu).

-export([
  sortkey/2
]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).

%% @type ustring() = binary()
%% @type rule() = ustring()
%% @type locale() = string()
%% @type collator() = locale() | rule()
%% @type sortkey() = binary()

-type ustring() :: binary().
-type rule() :: ustring().
-type locale() :: string().
-type collator() :: rule() | locale().
-type sortkey() :: binary().

-export_type([ustring/0, rule/0, locale/0]).

nif_stub_error(Line) ->
  erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).

init() ->
  PrivDir = case code:priv_dir(dictionary_maker) of
    {error, bad_name} ->
      EbinDir = filename:dirname(code:which(?MODULE)),
      AppPath = filename:dirname(EbinDir),
      filename:join(AppPath, "priv");
    Path -> Path
  end,
  erlang:load_nif(filename:join(PrivDir, icu_nif), 0).

%% @doc Takes a UString, a UFT-16 unicode string with the native endian
%% encoding. The UString is encoded as an erlang binary. See the icu4e
%% library for information on creating them, particularly ustring:new/2. The
%% other argument is either the target locate as an ascii/latin1 list string
%% or a tailoring rule as a UString.
-spec sortkey(Col :: collator(), UString :: ustring()) -> {ok, SortKey :: sortkey()} | {error, Reason :: string()}.
sortkey(Col, UString) when is_list(Col) ->
  locale_sort_key(Col, UString);
  
sortkey(Col, UString) when is_binary(Col) ->
  rule_sort_key(Col, UString).
 
 
locale_sort_key(_Locale, _UString) ->
  ?nif_stub.

rule_sort_key(_Rules, _UString) ->
  ?nif_stub.
