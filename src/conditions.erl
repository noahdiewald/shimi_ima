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

-export([expect/1, trans/1]).

expect(Conditions) ->
  expect(Conditions, [], []).
  
expect([], Acc1, Acc2) ->
  "{" ++ string:join(Acc1, ", ") ++ ", 'allVisited': function () {return (" ++ string:join(Acc2, " && ") ++ ")}}";

expect([H|Rest], Acc1, Acc2) ->
  case proplists:get_value(<<"field">>, H) of
    undefined -> expect(Rest, Acc1, Acc2);
    Fid ->
      Fid1 = lists:reverse(binary_to_list(Fid)),
      Fid2 = [$e,$s,$l,$a,$f,$ ,$: ,$'|Fid1],
      Fid3 = [$],$'|Fid1],
      expect(Rest, [[$'|lists:reverse(Fid2)]|Acc1], [[$t,$h,$i,$s,$[,$'|lists:reverse(Fid3)]|Acc2])
  end.
  
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
    <<"match">> -> build_expression(match, Condition);
    <<"member">> -> build_expression(member, Condition);
    <<"true">> -> build_expression(true, Condition);
    <<"blank">> -> build_expression(blank, Condition)
  end.

build_expression(greater, Condition) ->
  Exp = build_expression(">", "value", proplists:get_value(<<"argument">>, Condition)),
  negate(Exp, Condition);
  
build_expression(less, Condition) ->
  Exp = build_expression("<", "value", proplists:get_value(<<"argument">>, Condition)),
  negate(Exp, Condition);

build_expression(equal, Condition) ->
  Exp = build_expression("==", "value", proplists:get_value(<<"argument">>, Condition)),
  negate(Exp, Condition);

build_expression(true, Condition) ->
  Exp = build_expression("==", "value", "true"),
  negate(Exp, Condition);
  
build_expression(match, Condition) ->
  Exp = "((/" ++ 
        convert_binary(proplists:get_value(<<"argument">>, Condition)) ++ 
        "/).test(value))",
  negate(Exp, Condition);
  
build_expression(blank, Condition) ->
  Exp = "(value.isBlank())",
  negate(Exp, Condition);
  
build_expression(member, Condition) ->
  Exp = "(value.indexOf('" ++ 
        convert_binary(proplists:get_value(<<"argument">>, Condition)) ++ 
        "') > -1)",
  negate(Exp, Condition);
  
build_expression(field, Condition) ->
  build_expression("===", "fieldId", proplists:get_value(<<"field">>, Condition));
  
build_expression(fieldset, Condition) ->
  build_expression("===", "fieldsetId", proplists:get_value(<<"fieldset">>, Condition)).

build_expression(Operator, ScriptVar, Val) when is_binary(Val) ->
  build_expression(Operator, ScriptVar, "'" ++ convert_binary(Val) ++ "'");

build_expression(Operator, ScriptVar, Val) when is_integer(Val); is_float(Val) ->
  build_expression(Operator, ScriptVar, convert_number(Val));

build_expression(Operator, ScriptVar, Val) when is_list(Val) ->
  string:join(["(" ++ ScriptVar, Operator, Val ++ ")"], " ").

convert_binary(Bin) ->
  String = binary_to_list(Bin),
  escape_arg(String, []).

convert_number(Num) ->
  [String] = io_lib:format("~w", [Num]),
  String.
  
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
  Field = build_expression(field, Condition),
  "(!" ++ Field ++ " || " ++ join_and([Exp, Field]) ++ ")".

negate(Exp, Condition) ->  
  case proplists:get_value(<<"negate">>, Condition) of
    true -> join_expressions([$!|Exp], Condition);
    _ -> join_expressions(Exp, Condition)
  end.
  
join_and(Conditions) ->
  string:join(lists:reverse(Conditions), " && ").