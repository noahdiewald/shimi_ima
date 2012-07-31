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
%%% @doc Loop through the views continuously making get requests so
%%% that users aren't surprised by excessive lag time.

-module(view_updater).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([
         update_views/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

-spec update_views(string()) -> ok.
update_views(DB) ->
    Views = utils:shuffle(couch:get_views(DB)),
    gen_server:start({local, ?SERVER}, ?MODULE, {DB, Views}, []).

-spec init({string(), [binary()]}) -> {ok, {string(), [binary()]}}.
init(S) ->
    erlang:send_after(1000, ?MODULE, trigger),
    {ok, S}.

handle_call(_Request, _From, S) ->
    {noreply, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(trigger, S={_, []}) ->
    {stop, normal, S};
handle_info(trigger, {DB, Views}) ->
    erlang:send(?MODULE, {DB, Views}),
    {noreply, {DB, []}};
handle_info({_DB, []}, S) ->
    erlang:send_after(1000, ?MODULE, trigger),
    {noreply, S};
handle_info({DB, [Path|Rest]}, S) ->
    update_view(DB, Path),
    erlang:send_after(1000, ?MODULE, {DB, Rest}),
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

update_view(DB, Path) ->
    case couch:get_silent(DB, binary_to_list(Path)) of
        ok -> ok;
        {error, req_timedout} -> update_view(DB, Path)
    end.
