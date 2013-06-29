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
    trans(Conditions, [], false).

%% @doc Take a list of conditions as JSON terms plus an accumulator
%% and convert it in to a predicate string.
-spec trans(jsn:json_term(), [string()], boolean()) -> binary().
trans([], Acc, _) ->
    list_to_binary(lists:reverse(Acc));
trans(Conditions, Acc=[[$(,$e,$x|_]|_], true) ->
    simple_trans(Conditions, Acc, true);
trans(Conditions, Acc=["("|_], Ex) ->
    simple_trans(Conditions, Acc, Ex);
trans(Conditions, Acc=[")"|_], Ex) ->
    and_or(Conditions, Acc, Ex);
trans(Conditions, Acc=[" || "|_], Ex) ->
    simple_trans(Conditions, Acc, Ex);
trans(Conditions, Acc=[], Ex) ->
    simple_trans(Conditions, Acc, Ex);
trans(Conditions, Acc, Ex) ->
    and_or(Conditions, Acc, Ex).

exopener([Condition|_], false) ->
    Field = binary_to_list(proplists:get_value(<<"field">>, Condition)),
    "(existentialTest('" ++ Field ++ "'," ++ "function (x) {return ".

and_or([Condition|Conditions], Acc, Ex) ->
    case proplists:get_value(<<"is_or">>, Condition) of
        true ->
            [C2|Cs2] = Conditions,
            case proplists:get_value(<<"parens">>, C2) of
                <<"open">> -> trans(Cs2, ["(", " || "|Acc], Ex);
                <<"exopen">> -> trans(Cs2, [exopener(Cs2, Ex),
                                            " || "|Acc], true);
                false -> trans(Conditions, [" || "|Acc], Ex)
            end;
        false ->
            and_parens([Condition|Conditions], Acc, Ex)
    end.

and_parens([Condition|Conditions], Acc, Ex) ->
    case proplists:get_value(<<"parens">>, Condition) of
        <<"open">> -> trans(Conditions, ["(", " && "|Acc], Ex);
        <<"exopen">> -> trans(Conditions, [exopener(Conditions, Ex),
                                           " && "|Acc], true);
        <<"close">> -> trans(Conditions, [")"|Acc], Ex);
        <<"exclose">> -> trans(Conditions, [")", ";})"|Acc], false);
        false ->
            trans(Conditions, 
                  [build_expression(Condition, Ex), " && "|Acc], Ex)
    end.

simple_trans([Condition|Conditions], Acc, Ex) ->    
    case proplists:get_value(<<"parens">>, Condition) of
        <<"open">> -> trans(Conditions, ["("|Acc], Ex);
        <<"exopen">> -> trans(Conditions, 
                              [exopener(Conditions, Ex)|Acc], true);
        false -> trans(Conditions, 
                             [build_expression(Condition, Ex)|Acc], Ex)
    end.
    
%% @doc Takes a condition as a JSON term and converts it in to a
%% predicate string.
-spec build_expression(jsn:json_term(), boolean()) -> string().  
build_expression(Condition, Ex) ->
    Prefix = case proplists:get_value(<<"negate">>, Condition) of
                 true -> "!";
                 _ -> ""
             end,
    case proplists:get_value(<<"operator">>, Condition) of
        <<"equal">> -> build_expression(Prefix, "equals", Condition, Ex);
        <<"greater">> -> build_expression(Prefix, "greaterThan", Condition, Ex);
        <<"less">> -> build_expression(Prefix, "lessThan", Condition, Ex);
        <<"match">> -> build_expression(Prefix, "matches", Condition, Ex);
        <<"member">> -> build_expression(Prefix, "hasMember", Condition, Ex);
        <<"hasExactly">> -> 
            build_expression(Prefix, "hasExactly", Condition, false);
        <<"hasGreater">> -> 
            build_expression(Prefix, "hasGreater", Condition, false);
        <<"hasLess">> -> build_expression(Prefix, "hasLess", Condition, false);
        <<"isDefined">> -> 
            build_expression(Prefix, "isDefined", Condition, false);
        <<"true">> -> build_expression(Prefix, "isTrue", Condition, Ex);
        <<"blank">> -> build_expression(Prefix, "isBlank", Condition, Ex)
    end.

%% @doc Helper function for build_expression/1.
-spec build_expression(string(), string(), jsn:json_term(), boolean()) -> string().
build_expression(Prefix, Function=[$i,$s|_], Condition, Ex) ->
    "(" ++ Prefix ++ Function ++ "('" ++
        binary_to_list(proplists:get_value(<<"field">>, Condition)) ++ "'" ++ 
        add_ex(Ex) ++ "))";
build_expression(Prefix, Function, Condition, Ex) -> 
    "(" ++ Prefix ++ Function ++ "('" ++ 
        binary_to_list(proplists:get_value(<<"field">>, Condition)) ++ "'," ++
        process_arg(
          Function, proplists:get_value(<<"argument">>, Condition)) ++ 
        add_ex(Ex) ++ "))".

-spec add_ex(boolean()) -> string().
add_ex(true) ->
    ",x";
add_ex(_) ->
    "".

-spec process_arg(string(), binary()) -> string().
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
  
