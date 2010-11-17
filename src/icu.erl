-module(icu).
-export([
  get_sort_key/1, 
  get_sort_key/3
]).
-on_load(init/0).

init() ->
  File = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "dictionary_maker_drv"]),
  erlang:load_nif(File, 0).


get_sort_key(Bin) ->
  get_sort_key(locale, [], Bin).


get_sort_key(locale, Locale, Bin) ->
  Bin1 = unicode:characters_to_binary(Bin, utf8, {utf16,little}),
  locale_sort_key(Locale, Bin1);

get_sort_key(rule, Rule, Bin) ->
  Bin1 = unicode:characters_to_binary(Bin, utf8, {utf16,little}),
  Rule1 = unicode:characters_to_binary(Rule, utf8, {utf16,little}),
  rule_sort_key(Rule1, Bin1).
 
  
locale_sort_key(_Locale, _Bin) ->
  nif_error(?LINE).

  
rule_sort_key(_Rule, _Bin) ->
  nif_error(?LINE).


nif_error(Line) ->
  exit({nif_not_loaded, ?MODULE, line, Line}).
