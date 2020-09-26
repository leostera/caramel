% Source code generated with Caramel.
-module(holder).

-export([do_work/0]).
-export([handle_message/2]).
-export([loop/2]).
-export([start/1]).

-spec handle_message({binary(), integer()}, option:t({add, integer()}
            | {hello, binary()}
            | reset
            )) :: {binary(), integer()}.
handle_message(State, Msg) ->
  {X, Y} = State,
  case Msg of
    {some, reset} -> {<<"">>, 0};
    {some, {add, Z}} -> {X, Z};
    {some, {hello, N}} -> {N, Y};
    none -> State
  end.

-spec loop(fun((process:after_time()) -> option:t({add, integer()}
  | {hello, binary()}
  | reset
  )), {binary(), integer()}) :: A.
loop(Recv, State) ->
  io:format(<<"current_state: ~p\n">>, [State | []]),
  Msg = Recv({bounded, 5000}),
  State2 = handle_message(State, Msg),
  loop(Recv, State2).

-spec start({binary(), integer()}) :: erlang:pid({add, integer()}
   | {hello, binary()}
   | reset
   ).
start(X) -> process:make(fun
  (_self, Recv) -> loop(Recv, X)
end).

-spec do_work() :: ok.
do_work() ->
  Pid = start({<<"hi">>, 0}),
  erlang:send(Pid, {hello, <<"joe">>}).


