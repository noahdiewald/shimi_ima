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
%%% @doc Tests for fieldset functions

-module(document_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/types.hrl").
    
document_json() -> 
  [{<<"_id">>,<<"25250e2ead108a8f60213f24040007e4">>},
  {<<"_rev">>,<<"25250e2ead108a8f60213f24040007e4">>},
  {<<"description">>,<<"caltest">>},
  {<<"doctype">>,<<"CalTest">>},
  {<<"created_at_">>,<<"Tue, 08 Nov 2011 21:24:04 GMT">>},
  {<<"created_by_">>,<<"257e4">>},
  {<<"updated_at_">>,<<"Tue, 08 Nov 2011 21:24:04 GMT">>},
  {<<"updated_by_">>,<<"25250e4">>},
  {<<"prev_">>,<<"25250e2ead108a8f60213f24040007e4">>},
  {<<"deleted_">>,false},
  {<<"fieldsets">>,[fieldset_test:single_json(),fieldset_test:multiple_json()]}].

document() -> 
  #document{
    id = <<"25250e2ead108a8f60213f24040007e4">>,
    rev = <<"25250e2ead108a8f60213f24040007e4">>,
    description = <<"caltest">>,
    doctype = <<"CalTest">>,
    prev = <<"25250e2ead108a8f60213f24040007e4">>,
    created_by = <<"257e4">>,
    updated_by = <<"25250e4">>,
    deleted = false,
    created_at = {{2011,11,8},{21,24,04}},
    updated_at = {{2011,11,8},{21,24,04}},
    fieldsets = [fieldset_test:single_docfieldset(), fieldset_test:multiple_docfieldset()]}.

from_json_test_() ->
 [
   ?_assertEqual(document:from_json(document_json()), document())
 ].

to_json_test_() ->
 [
   ?_assertEqual(document:to_json(document()), document_json())
 ].
 