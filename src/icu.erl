-module(icu).
-export([get_sort_key/0]).
-on_load(init/0).

init() ->
  File = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "dictionary_maker_drv"]),
  
  erlang:load_nif(File, 0).

get_sort_key() ->
  nif_error(?LINE).

nif_error(Line) ->
  exit({nif_not_loaded, ?MODULE, line, Line}).
