% Source code generated with Caramel.
-module(looper).

-export([do_work/0]).
-export([loop/1]).
-export([start/1]).

loop(X) -> loop(X).

start(X) -> erlang:spawn(fun
  () -> loop(X)
end).

do_work() ->
  Pid = start(1),
  erlang:send(Pid, 2).


