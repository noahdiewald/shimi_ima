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
%%% @doc This module contains functions for manipulating fields

-module(field).

-export([
  from_json/2
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("include/config.hrl").
-include_lib("include/types.hrl").

%% @doc Convert a jsn:json_term() fieldset within a document to a
%% docfield()

-spec from_json(doc, Json :: jsn:json_term()) -> docfield().
from_json(doc, Json) ->
  Subcategory = get_subcategory(jsn:get_value(<<"subcategory">>, Json)),
  #docfield{
    id = jsn:get_value(<<"id">>, Json),
    instance = jsn:get_value(<<"instance">>, Json),
    charseq = jsn:get_value(<<"charseq">>, Json),
    name = jsn:get_value(<<"name">>, Json),
    label = jsn:get_value(<<"label">>, Json),
    order = jsn:get_value(<<"order">>, Json),
    head = jsn:get_value(<<"head">>, Json),
    max = jsn:get_value(<<"max">>, Json),
    min = jsn:get_value(<<"min">>, Json),
    regex = jsn:get_value(<<"regex">>, Json),
    required = jsn:get_value(<<"required">>, Json),
    reversal = jsn:get_value(<<"reversal">>, Json),
    subcategory = Subcategory,
    value = jsn:get_value(<<"value">>, Json),
    sortkey = jsn:get_value(<<"sortkey">>, Json)
  }.

-spec get_subcategory(binary()) -> subcategory().
get_subcategory(Bin) ->
  case Bin of
    <<"text">> -> text;
    <<"textarea">> -> textarea;
    <<"date">> -> date;
    <<"integer">> -> integer;
    <<"rational">> -> rational;
    <<"boolean">> -> boolean;
    <<"openboolean">> -> openboolean;
    <<"select">> -> select;
    <<"multiselect">> -> multiselect;
    <<"docselect">> -> docselect;
    <<"docmultiselect">> -> docmultiselect;
    <<"file">> -> file
  end.
