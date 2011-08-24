%% @author Noah Diewald <noah@diewald.me>
%% @copyright 2011 University of Wisconsin Madison Board of Regents.
%% Copyright (c) 2011 University of Wisconsin Madison Board of Regents
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

-module(validations_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(RW_USER, {struct, [{<<"roles">>, []}]}).

-define(RO_USER, {struct, [{<<"roles">>, [<<"readonly">>]}]}).

basic_test_() ->
  Fun = fun() ->
    {ok, P} = js_driver:new(),
    erlydtl:compile("../templates/javascript_validation.dtl", validation_template),
    {ok, Jiolist} = validation_template:render(),
    Jbin = iolist_to_binary(["var validate = "|Jiolist]),
    ?assertMatch(ok, js:define(P, Jbin)),
    ?assertMatch({ok, <<"Skipped">>}, js:call(P, <<"validate">>, [{struct, []}, {struct, []}, ?RW_USER])),
    erlang:unlink(P)
  end,
  my_setup(Fun).

readonly_test_() ->
  Fun = fun(P) ->
    {error, [_,{_,Msg},_]} = vcall(P, {struct, []}, ?RO_USER),
    ?assertMatch(<<"uncaught exception: undefined$is a read only user.$No ID">>, Msg)
  end,
  my_setup(get_environment(Fun)).

vcall(P, Args) ->
  vcall(Args, ?RW_USER).

vcall(P, Args, User) ->
  js:call(P, <<"validate">>, [Args, {struct, [{<<"testing">>, true}]}, User]).
  
my_setup(Fun) ->
  {setup, fun() -> erlang_js:start() end, Fun}.
  
get_environment() ->
  {ok, P} = js_driver:new(),
  erlydtl:compile("../templates/javascript_validation.dtl", validation_template),
  {ok, Jiolist} = validation_template:render(),
  Jbin = iolist_to_binary(["var validate = "|Jiolist]),
  js:define(P, Jbin),
  {ok, P}.

get_environment(Fun) ->
  fun() ->
    {ok, P} = get_environment(),
    Fun(P),
    erlang:unlink(P)
  end.
  