% Source code generated with Caramel.
-module(tiny).

-export([loop/2]).
-export([start/1]).

-spec loop(integer(), any()) :: any().
loop(T, Recv) ->
  io:format(<<"~p\n">>, [T | []]),
  timer:sleep(T),
  loop(erlang:'*'(T, 2), Recv).

-spec start(integer()) :: erlang:pid(any()).
start(T) -> process:make(fun
  (_self, R) -> loop(T, R)
end).


