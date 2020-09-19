% Source code generated with Caramel.
-module(holder).

-export([do_work/1]).
-export([handle_message/2]).
-export([loop/2]).
-export([start/1]).

handle_message(State, Msg) ->
  {X, Y} = State,
  case Msg of
    {some, reset} -> {<<"">>, 0};
    {some, {add, Z}} -> {X, Z};
    {some, {hello, N}} -> {N, Y};
    none -> State
  end.

loop(Recv, State) ->
  io:format(<<"current_state: ~p\n">>, [State | []]),
  Msg = Recv({bounded, 5000}),
  State2 = handle_message(State, Msg),
  loop(Recv, State2).

start(X) -> process:make(fun
  (_Self, Recv) -> loop(Recv, X)
end).

do_work() ->
  Pid = start({<<"hi">>, 0}),
  erlang:send(Pid, {hello, <<"joe">>}).


