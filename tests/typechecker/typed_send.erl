-module(typed_process).

-export([spawn_int/1, start/0]).

loop(Recv, S) ->
  receive
    {replace, X} -> loop(Recv, X);
    print -> _ = print_int(S), loop(Recv, S)
  end.

spawn_int(S) -> erlang:spawn(fun () -> loop(S) end).

start() ->
  spawn_int(0) ! {replace, <<"wrong value">>}.
