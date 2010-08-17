%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the dictionary_maker application.

-module(dictionary_maker_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for dictionary_maker.
start(_Type, _StartArgs) ->
    dictionary_maker_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for dictionary_maker.
stop(_State) ->
    ok.
