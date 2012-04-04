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
         convert/1,
         to_string/1
        ]).

-include_lib("types.hrl").

%% @doc A JavaScript date string format is converted to an Erlang
%% calendar:datetime()
convert([_D,_A,_Y,_SP,M,O,N,_SP,D1,D2,_SP,Y1,Y2,Y3,Y4,_SP,H1,H2,_Col,M1,M2,_Col,S1,S2|Rest]) ->
    Year = list_to_integer([Y1, Y2, Y3, Y4]),
    Day = list_to_integer([D1, D2]),
    Month = http_util:convert_month([M, O, N]),
    Hour = list_to_integer([H1, H2]),
    Min = list_to_integer([M1, M2]),
    Sec = list_to_integer([S1, S2]),
    convert2({{Year, Month, Day}, {Hour, Min, Sec}}, Rest);
convert(Datetime) ->
    httpd_util:convert_request_date(Datetime).

-spec convert2(calendar:datetime(), string()) -> {ok, calendar:datetime()} | bad_date.
convert2(Datetime, [$\ ,$G,$M,$T,Sign,Hr1,Hr2,Min1,Min2|_Rest]) ->
    HrOffset = list_to_integer([Hr1,Hr2]),
    MinOffset = list_to_integer([Min1,Min2]),
    convert3(Datetime, Sign, HrOffset, MinOffset);
convert2(Datetime, _) ->
    {ok, Datetime}.

-spec convert3(calendar:datetime(), char(), integer(), integer()) -> {ok, calendar:datetime()} | bad_date.
convert3(Datetime, $+, HrOffset, MinOffset) -> 
    convert3(Datetime, $-, HrOffset * -1, MinOffset * -1);
convert3(Datetime, _, HrOffset, MinOffset) ->
    Secs = calendar:datetime_to_gregorian_seconds(Datetime) + 
        (HrOffset * 3600) + (MinOffset * 60),    
    {ok, calendar:gregorian_seconds_to_datetime(Secs)}.

%% @doc Produce a JavaScript "UTC" date string from a calendar:datetime()
-spec to_string(Datetime :: calendar:datetime()) -> {ok, string()}.
to_string(Datetime) ->
    erlydtl_dateformat:format(Datetime, "D, d M Y H:i:s") ++ " GMT".
