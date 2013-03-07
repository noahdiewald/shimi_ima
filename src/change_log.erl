%%% Copyright 2011 University of Wisconsin Madison Board of Regents.
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
%%% @doc For manipulating indexes.

-module(change_log).
-author('Noah Diewald <noah@diewald.me>').

-export([
    created/5,
    deleted/5,
    restored/5,
    updated/5
    ]).

created(Id, Rev, Doctype, Project, S) ->
    io:format("CREATED~n"),
    {Id, Rev, Doctype, Project, S}.

deleted(Doc, Rev, Doctype, Project, S) ->
    io:format("DELETED~n"),
    {Doc, Rev, Doctype, Project, S}.

restored(Doc, Rev, Doctype, Project, S) ->
    io:format("RESTORED~n"),
    {Doc, Rev, Doctype, Project, S}.

updated(Doc, Rev, Doctype, Project, S) ->
    io:format("UPDATED~n"),
    {Doc, Rev, Doctype, Project, S}.
