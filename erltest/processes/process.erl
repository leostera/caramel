% Source code generated with Caramel.
-module(process).
-export_type([after_time/0]).
-export_type([recv/1]).

-export([contramap/2]).
-export([make/1]).
-export([recv/1]).
-export([send/2]).

-type after_time() :: infinity
                    | {bounded, integer()}
                    .

-type recv(Message) :: fun((after_time()) -> option:t(Message)).

recv(Timeout) ->
  F = fun (T) -> receive X -> {some, X} after T -> none end end,
  case Timeout of
    infinity -> F(infinity);
    {bounded, T} -> F(T)
  end.

make(F) -> erlang:spawn(fun
  () ->
  Pid = erlang:self(),
  F(Pid, fun recv/1)
end).

send(Proc, Msg) -> erlang:send(Proc, Msg).

contramap(F, Pid) -> make(fun
  (_Self, Recv) ->
  case Recv(infinity) of
    {some, A} -> send(Pid, F(A));
    none -> {}
  end
end).


