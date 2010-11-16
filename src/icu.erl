-module(icu).
-export([
  get_sort_key/1, 
  get_sort_key/2,
  locale_sort_key/2,
  tailoring_sort_key/2
]).
-on_load(init/0).

init() ->
  File = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "dictionary_maker_drv"]),
  erlang:load_nif(File, 0).

get_sort_key(Bin) ->
  locale_sort_key([], Bin).

get_sort_key(Locale, Bin) ->
  locale_sort_key(Locale, Bin).
  
locale_sort_key(_Locale, _Bin) ->
  nif_error(?LINE).
  
tailoring_sort_key(_Tailoring, _Bin) ->
  nif_error(?LINE).

nif_error(Line) ->
  exit({nif_not_loaded, ?MODULE, line, Line}).
