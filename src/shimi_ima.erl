%%% Copyright 2012 University of Wisconsin Madison Board of Regents.
%%%
%%% This file is part of Ʃimi Ima.
%%%
%%% dictionary_maker is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% dictionary_maker is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General
%%% Public License along with dictionary_maker. If not, see
%%% <http://www.gnu.org/licenses/>.

%%% @copyright 2012 University of Wisconsin Madison Board of Regents.
%%% @version {@version}
%%% @author Noah Diewald <noah@diewald.me>
%%% @doc shimi_ima startup code

-module(shimi_ima).
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
    ensure_started(sasl),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(ibrowse),
    ensure_started(ranch),
    ensure_started(cowboy),
    shimi_ima_sup:start_link().

%% @spec start() -> ok
%% @doc Start the dictionary_maker server.
start() ->
    ensure_started(crypto),
    ensure_started(sasl),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(ibrowse),
    ensure_started(ranch),
    ensure_started(cowboy),
    write_pid_file(),
    ensure_started(shimi_ima).

%% @spec stop() -> ok
%% @doc Stop the dictionary_maker server.
stop() ->
    Res = application:stop(shimi_ima),
    application:stop(cowboy),
    application:stop(mochiweb),
    application:stop(ranch),
    application:stop(ibrowse),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(public_key),
    application:stop(sasl),
    application:stop(crypto),
    Res.

%% @spec stop([Node]) -> void()
%% @doc Stop a dictionary maker server on a specific node
stop([Node]) ->
    io:format("Stop:~p~n",[Node]),
    case net_adm:ping(Node) of
     pong -> 
       % TODO: This could delete the pid file even if the program hasn't stoped.
       % TODO: Do not hard code this file name
       % TODO: Build file name in a portable manner
       file:delete("./shimi_ima.pid"),
       rpc:cast(Node, init, stop, []);
     pang -> io:format("There is no node with this name~n")
    end,
    init:stop().

write_pid_file() ->
    case file:open("./shimi_ima.pid", [write]) of
        {ok, Fd} ->
            io:format(Fd, "~s~n", [os:getpid()]),
            file:close(Fd);
        {error, Reason} ->
            throw({cannot_write_pid_file, "./shimi_ima.pid", Reason})
        end.

