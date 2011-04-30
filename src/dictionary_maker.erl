%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 University of Wisconsin Madison Board of Regents.
%% Copyright (c) 2010 University of Wisconsin Madison Board of Regents
%%
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
%% THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%% @doc dictionary_maker startup code

-module(dictionary_maker).
-author('Noah Diewald <noah@diewald.me>').
-export([start/0, start_link/0, stop/0, stop/1]).

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
    write_pid_file(),
    application:start(dictionary_maker).

%% @spec stop() -> ok
%% @doc Stop the dictionary_maker server.
stop() ->
    Res = application:stop(dictionary_maker),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    Res.

%% @spec stop([Node]) -> void()
%% @doc Stop a dictionary maker server on a specific node
stop([Node]) ->
    io:format("Stop:~p~n",[Node]),
    case net_adm:ping(Node) of
     pong -> 
       % TODO: this could delete the pid file even if the program hasn't stoped.
       file:delete("./dictionary_maker.pid"),
       rpc:cast(Node, init, stop, []);
     pang -> io:format("There is no node with this name~n")
    end,
    init:stop().

write_pid_file() ->
    case file:open("./dictionary_maker.pid", [write]) of
        {ok, Fd} ->
            io:format(Fd, "~s~n", [os:getpid()]),
            file:close(Fd);
        {error, Reason} ->
            throw({cannot_write_pid_file, "./dictionary_maker.pid", Reason})
        end.

