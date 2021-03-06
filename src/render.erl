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
%%% @doc Utilities and helpers for rendering html and json using erlydtl

-module(render).

-export([
  renderings/2,
  render/2
]).

renderings(Json, Template) ->
    Rows = jsn:get_value(<<"rows">>, Json),
    F = fun(X) ->
                {ok, Y} = render(Template, X),
                Y
        end,
    lists:map(F, Rows).
  
render(Template, Params) ->
    Template:render([{devel, utils:is_devel()}|Params]).
