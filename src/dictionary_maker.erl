%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc dictionary_maker startup code

-module(dictionary_maker).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    dictionary_maker_sup:start_link().

%% @spec start() -> ok
%% @doc Start the dictionary_maker server.
start() ->
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(dictionary_maker).

%% @spec stop() -> ok
%% @doc Stop the dictionary_maker server.
stop() ->
    Res = application:stop(dictionary_maker),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    Res.
