% Source code generated with Caramel.
-module(holder_annotated).
-export_type([msg/0]).
-export_type([state/0]).

-export([do_work/0]).
-export([handle_message/2]).
-export([loop/2]).
-export([start/1]).

-type msg() :: reset
             | {add, integer()}
             | {hello, binary()}
             .

-type state() :: {binary(), integer()}.

-spec handle_message(state(), option:t(msg())) :: state().
handle_message(_, _) ->
  {X, Y} = ,
  case  of
    {some, reset} -> {<<"">>, 0};
    {some, {add, Z}} -> {X, Z};
    {some, {hello, N}} -> {N, Y};
    none -> 
  end.

-spec loop(fun((process:after_time()) -> option:t(msg())), state()) :: any().
loop(Recv, State) ->
  io:format(<<"current_state: ~p\n">>, [State | []]),
  Msg = Recv({bounded, 5000}),
  State2 = handle_message(State, Msg),
  loop(Recv, State2).

-spec start(state()) :: erlang:pid(msg()).
start(X) -> process:make(fun
  (_self, Recv) -> loop(Recv, X)
end).

-spec do_work() :: ok.
do_work() ->
  Pid = start({<<"hi">>, 0}),
  erlang:send(Pid, {hello, <<"joe">>}).


