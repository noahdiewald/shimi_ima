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
%%% @doc This module contains functions for touching documents. In
%%% this context "touching" refers to a maintenance action where all
%%% documents are updated and made to conform to any changes in
%%% fieldsets, fields, charseqs, etc.

-module(untouched).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([
         delete/2,
         exists/1,
         get/1,
         start/2
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

% API

start(Doctype, Documents) ->
    Server = me(Doctype),
    gen_server:start({local, Server}, ?MODULE, Documents, []).

get(Doctype) ->
    gen_server:call(me(Doctype), get).

exists(Doctype) ->
    lists:member(me(Doctype), registered()).

delete(Doctype, Id) ->
    gen_server:cast(me(Doctype), {delete, Id}).

% Gen Server

init(Documents) ->
    F = fun([{<<"id">>, Id}|_]) ->
                Id
        end,
    Ids = lists:map(F, Documents),
    {ok, Ids}.

handle_call(get, _From, Ids) ->
    {reply, Ids, Ids};
handle_call(_Msg, _From, Ids) ->
    {noreply, Ids}.

handle_cast({delete, Id}, Ids) ->
    NewIds = lists:delete(Id, Ids),
    case NewIds of
        [] -> {stop, normal, NewIds};
        _ -> {noreply, NewIds}
    end;
handle_cast(_Msg, Ids) ->
    {noreply, Ids}.

handle_info(_Msg, Ids) ->
    {noreply, Ids}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

% Private 

me(Doctype) ->
    list_to_atom(atom_to_list(?SERVER) ++ "-" ++ Doctype).
