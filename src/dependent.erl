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
%%% @doc Finding and manipulating dependent documents.

-module(dependent).

-export([
         find/3,
         bump/3
        ]).

-include_lib("include/types.hrl").

-spec find(string(), utils:req_data(), any()) -> [jsn:json_term()].
find(Doctype, Project, S) ->
    F = fun (X, Acc) -> find_f(X, Acc, Doctype) end,
    Qs = view:from_list([{<<"include_docs">>, true}]),
    couch:fold_view("shimi_ima", "all_fieldsets", view:to_string(Qs), F, Project, S).

-spec bump(string(), utils:req_data(), any()) -> ok.
bump(Doctype, Project, S) ->
    Doctypes = find(Doctype, Project, S),
    bump_all(Doctypes, Project, S).

% Helper Functions

-spec bump_all([string()], utils:req_data(), any()) -> ok.
bump_all([], _R, _S) ->
    ok;
bump_all([H|T], Project, S) ->
    {ok, Json} = h:get(H, Project, S),
    {ok, _} = couch:update(binary_to_list(jsn:get_value(<<"_id">>, Json)), Json, Project, S),
    bump_all(T, Project, S).

-spec find_f(jsn:json_term(), jsn:json_term(), string()) -> jsn:json_term().
find_f(X, Acc, ODoctype) ->
    case jsn:get_value(<<"key">>, X) of
        [TDoctype, _, <<"fieldset-field">>, _] ->
            case is_dependent(jsn:get_value(<<"doc">>, X), ODoctype) of
                false ->
                    Acc;
                true ->
                    ordsets:add_element(binary_to_list(TDoctype), Acc)
            end;
        _Else ->
            Acc
    end.

-spec is_dependent(jsn:json_term(), string()) -> boolean().
is_dependent(Doc, Doctype) -> 
    Subcat = jsn:get_value(<<"subcategory">>, Doc),
    case is_dependent_subcat(Subcat) of
        true ->
            jsn:get_value(<<"source">>, Doc) =:= list_to_binary(Doctype);
        false ->
            false
    end.

-spec is_dependent_subcat(binary()) -> boolean().
is_dependent_subcat(<<"docmultiselect">>) ->
    true;
is_dependent_subcat(<<"docselect">>) ->
    true;
is_dependent_subcat(_) ->
    false.
