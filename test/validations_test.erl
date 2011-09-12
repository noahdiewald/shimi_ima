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

date_test_() ->
  my_setup([fun () -> Test(get_thing()) end || Test <- [
    fun(P) -> % Test read only user validation
      {error, [_,{_,Msg},_]} = vcall(P, {struct, []}, ?RO_USER),
      ?assertEqual(<<"uncaught exception: undefined$is a read only user.$No ID">>, Msg)
    end,
    fun(P) -> % Test skipped document
      ?assertEqual({ok, <<"Skipped">>}, js:call(P, <<"validate">>, [{struct, []}, {struct, []}, ?RW_USER]))
    end,
    fun(P) -> % null saveDoc is ok
      ?assertEqual({ok, <<"Skipped">>}, js:call(P, <<"validate">>, [{struct, []}, null, ?RW_USER]))
    end,
    fun(P) -> % Test valid document
      ?assertEqual({ok, <<"Valid">>}, vcall(P, from_file("simple_doc")))
    end,
    
    % Integer Tests

    fun(P) -> % Integer too high
      ?assertMatch({error, [_,{_,<<"uncaught exception: ff$4d915decf693d51ab06a2f10920cb7ee$Must be less than or equal to 0">>},_]}, vcall(P, from_file("high_integer")))
    end,
    
    % Date Tests
    
    fun(P) -> % Bad date format
      ?assertMatch({error, [_,{_,<<"uncaught exception: caltest$25250e2ead108a8f60213f2404005d38$date must be in format yyyy-mm-dd">>},_]}, vcall(P, from_file("bad_format_doc")))
    end,
    fun(P) -> % Date too early
      ?assertMatch({error, [_,{_,<<"uncaught exception: caltest$25250e2ead108a8f60213f2404005d38$date must be later than 1990-09-23.">>},_]}, vcall(P, from_file("early_doc")))
    end,
    fun(P) -> % Date too late
      ?assertMatch({error, [_,{_,<<"uncaught exception: caltest$25250e2ead108a8f60213f2404005d38$date must be earlier than or equal to 1990-07-23.">>},_]}, vcall(P, from_file("late_doc")))
    end,
    fun(P) -> % Date in past
      {{Y,M,D},_} = calendar:local_time(),
      V = iolist_to_binary(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w", [Y - 1, M, D])),
      ?assertMatch({error, [_,{_,<<"uncaught exception: caltest$25250e2ead108a8f60213f2404005d38$date must be in the future.">>},_]}, vcall(P, from_template("past_doc", [{"value", V}])))
    end,
    fun(P) -> % Date in future
      {{Y,M,D},_} = calendar:local_time(),
      V = iolist_to_binary(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w", [Y + 1, M, D])),
      ?assertMatch({error, [_,{_,<<"uncaught exception: caltest$25250e2ead108a8f60213f2404005d38$date must be in the past.">>},_]}, vcall(P, from_template("future_doc", [{"value", V}])))
    end,
    fun(P) -> % Exactly today
      {{Y,M,D},_} = calendar:local_time(),
      V = iolist_to_binary(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w", [Y, M, D])),
      ?assertEqual({ok, <<"Valid">>}, vcall(P, from_template("today_doc", [{"value", V}])))
    end
  ]]).
  
vcall(P, Args) ->
  vcall(P, Args, ?RW_USER).

vcall(P, Args, User) ->
  js:call(P, <<"validate">>, [Args, {struct, [{<<"testing">>, true}]}, User]).
  
my_setup(Fun) ->
  {setup, 
    fun() -> 
      erlang_js:start(),
      port_setup(),
      my_env()
    end, 
    fun port_teardown/1,
    Fun}.
  
my_env() ->
  P = get_thing(),
  erlydtl:compile("../templates/javascript_validation.dtl", validation_template),
  {ok, Jiolist} = validation_template:render(),
  Jbin = iolist_to_binary(["var validate = "|re:replace(Jiolist, <<"\\\\\\\\">>, <<"\\\\">>, [global])]),
  js:define(P, Jbin),
  P.

from_file(Filename) ->
  {ok, Bin} = file:read_file("../test/files/" ++ Filename ++ ".json"),
  mochijson2:decode([Bin]).

from_template(Template, Args) ->
  erlydtl:compile("../test/files/" ++ Template ++ ".dtl", mytemplate),
  {ok, IoData} = mytemplate:render(Args),
  mochijson2:decode(IoData).

% Below borrowed from Basho
  
port_setup() ->
    port_setup(8).

port_setup(Size) ->
  {ok, P} = js_driver:new(8, Size),
  start_thing_holder(P),
  P.

port_teardown(P) ->
  thing_holder ! stop,
  case erlang:port_info(P) of
    undefined -> ok;
    _ ->
      erlang:port_connect(P, self()),
      js_driver:destroy(P)
  end.

null_teardown(_) ->
  thing_holder ! stop,
  ok.

get_thing() ->
  thing_holder ! {get_thing, self()},
  receive
    Thing ->
      if
        is_port(Thing) ->
          erlang:port_connect(Thing, self());
        true ->
          ok
      end,
      Thing
  end.

%% Internal functions
start_thing_holder(Thing) ->
  if
    is_port(Thing) ->
      erlang:unlink(Thing);
    true ->
      ok
  end,
  Pid = spawn(fun() -> thing_holder(Thing) end),
  register(thing_holder, Pid),
  Pid.

thing_holder(Thing) ->
  receive
    {get_thing, Caller} ->
      Caller ! Thing,
      thing_holder(Thing);
    stop ->
      ok
  end.

  