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
%%% @doc Custom filters for use with erlydtl templates.

-module(erlydtl_custom_filters).

-export([decode64/1,decode_json/1,decode/1]).

-spec decode64(binary()) -> iolist().
decode64([34, Bin, 34]) when is_binary(Bin) ->
    decode64(Bin);
decode64(Bin) when is_binary(Bin) ->
    base64:decode(Bin).

-spec decode_json(iolist()) -> jsn:json_term().
decode_json(IOList) ->
    jsn:decode(IOList).

-spec decode(binary()|iolist()) -> jsn:json_term().
decode([34, Bin, 34]) when is_binary(Bin) ->
    decode(Bin);
decode(IOList) ->
    jsn:from_base64(IOList).
