% Source code generated with Caramel.
-module(typed_reply).

-export([a_loop/3]).
-export([b_loop/3]).
-export([c_loop/3]).
-export([run/0]).

a_loop(Pid, Recv, I) ->
  case Recv(infinity) of
    none -> a_loop(Pid, Recv, I);
    {some, {call, {Cb, Msg}}} -> _ = erlang:send(Cb, I),
a_loop(Pid, Recv, erlang:'+'(I, Msg))
  end.

b_loop(Pid, Recv, A) ->
  _ = timer:sleep(1000),
  _ = erlang:send(A, {call, {Pid, 1}}),
  case Recv({bounded, 0}) of
    none -> b_loop(Pid, Recv, A);
    {some, I} -> _ = io:format(<<"received: ~p, state must be ~p\n">>, [I | [erlang:'+'(I, 1) | []]]),
b_loop(Pid, Recv, A)
  end.

c_loop(Pid, Recv, A) ->
  _ = timer:sleep(1000),
  Cb = process:contramap(fun
  (X) -> erlang:'>'(X, 10)
end, Pid),
  _ = erlang:send(A, {call, {Cb, 1}}),
  case Recv({bounded, 0}) of
    none -> c_loop(Pid, Recv, A);
    {some, true} -> io:format(<<"yay, it is true\n">>, []);
    {some, false} -> _ = io:format(<<"oh noes!\n">>, []),
c_loop(Pid, Recv, A)
  end.

run() ->
  A = process:make(fun
  (Pid, Recv) -> a_loop(Pid, Recv, 0)
end),
  _ = process:make(fun
  (Pid, Recv) -> b_loop(Pid, Recv, A)
end),
  _ = process:make(fun
  (Pid, Recv) -> c_loop(Pid, Recv, A)
end),
  {}.


