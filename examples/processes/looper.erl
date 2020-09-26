% Source code generated with Caramel.
-module(looper).

-export([do_work/0]).
-export([loop/1]).
-export([start/1]).

-spec loop(A) :: B.
loop(X) ->
  io:format(<<"~p\n">>, [erlang:self() | []]),
  loop(X).

-spec start(A) :: erlang:pid(B).
start(X) -> erlang:spawn(fun
  () -> loop(X)
end).

-spec do_work() :: ok.
do_work() ->
  Pid = start(1),
  erlang:send(Pid, 2112).


