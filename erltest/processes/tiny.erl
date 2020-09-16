% Source code generated with Caramel.
-module(tiny).

-export([loop/2]).
-export([start/1]).

loop(T, Recv) ->
  _ = io:format(<<"~p\n">>, [T | []]),
  _ = timer:sleep(T),
  loop(erlang:'*'(T, 2), Recv).

start(T) -> process:spawn(fun
  (R) -> loop(T, R)
end).


