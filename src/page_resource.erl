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
%%% @doc The is the resource used for managing pages.

-module(page_resource).

-export([init/3]).
-export([
         allowed_methods/2,
         content_types_provided/2,
         main_html/2,
         rest_init/2
        ]).

init(_Transport, _R, _S) -> {upgrade, protocol, cowboy_rest}.

rest_init(R, S) -> {ok, R, S}.

allowed_methods(R, S) ->
    {[<<"HEAD">>, <<"GET">>], R, S}.
  
content_types_provided(R, S) ->
    {[{{<<"text">>, <<"html">>, []}, main_html}], R, S}.
  
main_html(R, S) ->
    {ok, Html} = render:render(main_dtl, []),
    {Html, R, S}.
