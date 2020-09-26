% Source code generated with Caramel.
-module(process).
-export_type([after_time/0]).
-export_type([recv/1]).

-export([contramap/2]).
-export([recv/1]).
-export([send/2]).
-export([spawn/1]).

-type after_time() :: infinity
                    | {bounded, integer()}
                    .

-type recv(M) :: fun((after_time()) -> option:t(M)).

-spec recv(after_time()) -> option:t(A).
recv(Timeout) ->
  F = fun (T) -> receive X -> {some, X} after T -> none end end,
  case Timeout of
    infinity -> F(infinity);
    {bounded, T} -> F(T)
  end.

-spec spawn(fun((erlang:pid(M), recv(M)) -> A)) -> erlang:pid(M).
spawn(F) -> erlang:spawn(fun
  () ->
  Pid = erlang:self(),
  F(Pid, fun recv/1)
end).

-spec send(erlang:pid(M), M) -> ok.
send(Proc, Msg) -> erlang:send(Proc, Msg).

-spec contramap(fun((B) -> A), erlang:pid(A)) -> erlang:pid(B).
contramap(F, Pid) -> spawn(fun
  (_self, Recv) ->
  case Recv(infinity) of
    {some, A} -> send(Pid, F(A));
    none -> ok
  end
end).


