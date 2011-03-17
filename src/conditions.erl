%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2010 University of Wisconsin Madison Board of Regents.
%% Copyright (c) 2010 University of Wisconsin Madison Board of Regents
%%
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
%% THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% @doc Convert an array of condition proplists to JavaScript expression.

-module(conditions).

-export([trans/1]).

trans(Conditions) ->
  trans(Conditions, [], []).

trans([], [], Acc2) ->
  string:join(lists:reverse(Acc2), " || ");
  
trans([], Acc1, Acc2) ->
  trans([], [], [join_and(Acc1)|Acc2]);
    
trans([Condition|Conditions], Acc1, Acc2) ->
  case proplists:get_value(<<"is_or">>, Condition) of
    true ->
      trans(Conditions, [], [join_and(Acc1)|Acc2]);
    _ ->
      trans(Conditions, [build_expression(Condition)|Acc1], Acc2)
  end.
  
build_expression(Condition) ->
  case proplists:get_value(<<"operator">>, Condition) of
    <<"equal">> -> build_expression(equal, Condition);
    <<"greater">> -> build_expression(greater, Condition);
    <<"less">> -> build_expression(less, Condition);
    <<"match">> -> build_expression(match, Condition)
  end.

build_expression(greater, Condition) ->
  Exp = build_expression(">", "value", proplists:get_value(<<"argument">>, Condition)),
  negate(Exp, Condition);
  
build_expression(less, Condition) ->
  Exp = build_expression("<", "value", proplists:get_value(<<"argument">>, Condition)),
  negate(Exp, Condition);
  
build_expression(match, Condition) ->
  Exp = "((/" ++ convert_binary(proplists:get_value(<<"argument">>, Condition)) ++ "/).test(value))",
  negate(Exp, Condition);

build_expression(equal, Condition) ->
  Exp = build_expression(equal, "value", proplists:get_value(<<"argument">>, Condition)),
  negate(Exp, Condition);
  
build_expression(field, Condition) ->
  build_expression(equal, "fieldId", proplists:get_value(<<"field">>, Condition));
  
build_expression(fieldset, Condition) ->
  build_expression(equal, "fieldsetID", proplists:get_value(<<"fieldset">>, Condition)).

build_expression(equal, ScriptVar, Val) ->
  build_expression("==", ScriptVar, Val);

build_expression(Operator, ScriptVar, Val) ->
  string:join(["(" ++ ScriptVar, Operator, "'" ++ convert_binary(Val) ++ "')"], " ").

convert_binary(Bin) ->
  String = binary_to_list(Bin),
  escape_arg(String, []).
  
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
  
join_expressions(Exp, Condition) ->    
  FieldsAndSets = [build_expression(field, Condition), build_expression(fieldset, Condition)],
  FieldsAndSetsString = "!(" ++ join_and(FieldsAndSets) ++ ")",
  "(" ++ FieldsAndSetsString ++ " || " ++ Exp ++ ")".

negate(Exp, Condition) ->  
  case proplists:get_value(<<"negate">>, Condition) of
    true -> join_expressions([$!|Exp], Condition);
    _ -> join_expressions(Exp, Condition)
  end.
  
join_and(Conditions) ->
  string:join(lists:reverse(Conditions), " && ").