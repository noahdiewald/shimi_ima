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
%%% @doc This module is for parsing JavaScript string representations of dates.

-module(js_date).

-export([
  convert/1
]).

-include_lib("include/types.hrl").

%% @doc A JavaScript date string format is converted to an Erlang
%% calendar:datetime()

-spec convert(Date :: string()) -> calendar:date() | bad_date.
convert([_D,_A,_Y,_SP,M,O,N,_SP,D1,D2,_SP,Y1,Y2,Y3,Y4,_SP,H1,H2,_Col,M1,M2,_Col,S1,S2|_Rest]) ->
  Year = list_to_integer([Y1, Y2, Y3, Y4]),
  Day = list_to_integer([D1, D2]),
  Month = http_util:convert_month([M, O, N]),
  Hour = list_to_integer([H1, H2]),
  Min = list_to_integer([M1, M2]),
  Sec = list_to_integer([S1, S2]),
  {ok, {{Year, Month, Day}, {Hour, Min, Sec}}};
convert(Date) ->
  httpd_util:convert_request_date(Date).
