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
%%% @doc Convert an array of condition proplists to JavaScript
%%% expression.

-module(conditions).

-export([trans/1]).

%% @doc Take a list of conditions as JSON terms and convert it in to a
%% predicate string.
-spec trans(jsn:json_term()) -> binary().
trans(Conditions) ->
    trans(Conditions, []).

%% @doc Take a list of conditions as JSON terms plus an accumulator
%% and convert it in to a predicate string.
-spec trans(jsn:json_term(), [string()]) -> binary().
trans([], Acc) ->
    list_to_binary(lists:reverse(Acc));
trans([Condition|Conditions], Acc=["("|_]) ->
    trans(Conditions, [build_expression(Condition)|Acc]);
trans([Condition|Conditions], Acc=[")"|_]) ->
    trans(Conditions, [build_expression(Condition)|Acc]);
trans([Condition|Conditions], Acc=[" || "|_]) ->
    case proplists:get_value(<<"parens">>, Condition) of
        <<"open">> -> trans(Conditions, ["("|Acc]);
        _ -> trans(Conditions, [build_expression(Condition)|Acc])
    end;
trans([Condition|Conditions], Acc=[]) ->
    case proplists:get_value(<<"parens">>, Condition) of
        <<"open">> -> trans(Conditions, ["("|Acc]);
        _ -> trans(Conditions, [build_expression(Condition)|Acc])
    end;
trans([Condition|Conditions], Acc) ->
    case proplists:get_value(<<"is_or">>, Condition) of
        true ->
            [C2|Cs2] = Conditions,
            case proplists:get_value(<<"parens">>, C2) of
                <<"open">> -> trans(Cs2, ["(", " || "|Acc]);
                _ -> trans(Conditions, [" || "|Acc])
            end;
        _ ->
            case proplists:get_value(<<"parens">>, Condition) of
                <<"open">> -> trans(Conditions, ["(", " && "|Acc]);
                <<"close">> -> trans(Conditions, [")"|Acc]);
                _ ->
                    trans(Conditions, [build_expression(Condition), " && "|Acc])
            end
    end.

%% @doc Takes a condition as a JSON term and converts it in to a
%% predicate string.
-spec build_expression(jsn:json_term()) -> string().  
build_expression(Condition) ->
    Prefix = case proplists:get_value(<<"negate">>, Condition) of
                 true -> "!";
                 _ -> ""
             end,
    case proplists:get_value(<<"operator">>, Condition) of
        <<"equal">> -> build_expression(Prefix, "equals", Condition);
        <<"greater">> -> build_expression(Prefix, "greaterThan", Condition);
        <<"less">> -> build_expression(Prefix, "lessThan", Condition);
        <<"match">> -> build_expression(Prefix, "matches", Condition);
        <<"member">> -> build_expression(Prefix, "hasMember", Condition);
        <<"hasExactly">> -> build_expression(Prefix, "hasExactly", Condition);
        <<"hasGreater">> -> build_expression(Prefix, "hasGreater", Condition);
        <<"hasLess">> -> build_expression(Prefix, "hasLess", Condition);
        <<"isDefined">> -> build_expression(Prefix, "isDefined", Condition);
        <<"true">> -> build_expression(Prefix, "isTrue", Condition);
        <<"blank">> -> build_expression(Prefix, "isBlank", Condition)
    end.

%% @doc Helper function for build_expression/1.
-spec build_expression(string(), string(), jsn:json_term()) -> string().
build_expression(Prefix, Function=[$i,$s|_], Condition) ->
    "(" ++ Prefix ++ Function ++ "('" ++
        binary_to_list(proplists:get_value(<<"field">>, Condition)) ++ "'))";
build_expression(Prefix, Function, Condition) -> 
    "(" ++ Prefix ++ Function ++ "('" ++ 
        binary_to_list(proplists:get_value(<<"field">>, Condition)) ++ "'," ++
        process_arg(
          Function, proplists:get_value(<<"argument">>, Condition)) ++ "))".

-spec process_arg(string(), binary()) -> string().
process_arg("matches", Arg) ->
    "/" ++ convert_binary(Arg) ++ "/";
process_arg(_, Arg) when is_integer(Arg); is_float(Arg) ->
    convert_number(Arg);
process_arg(_, Arg) ->
    "'" ++ convert_binary(Arg) ++ "'".

-spec convert_binary(binary()) -> string().
convert_binary(Bin) ->
  String = binary_to_list(Bin),
  escape_arg(String, []).

-spec convert_number(float()|integer()) -> string().
convert_number(Num) ->
  [String] = io_lib:format("~w", [Num]),
  String.
  
-spec escape_arg(string(), list()) -> string().
escape_arg([], Acc) ->
  lists:reverse(Acc);
  
escape_arg([$\\|Rest], Acc) ->
  escape_arg(Rest, [$\\,$\\|Acc]);
  
escape_arg([$'|Rest], Acc) ->
  escape_arg(Rest, [$',$\\|Acc]);
  
escape_arg([$"|Rest], Acc) ->
  escape_arg(Rest, [$",$\\|Acc]);
  
escape_arg([H|Rest], Acc) ->
  escape_arg(Rest, [H|Acc]).
  
