  $ cat >fib.erl <<EOF
  > -module(fib).
  > -export([fib/1]).
  > fib(0) -> 0;
  > fib(1) -> 1;
  > fib(N) -> erlang:'+'(fib(erlang:'-'(N,1)), fib(erlang:'-'(N,2))).
  > fib() -> fib(no).
  > EOF
  $ caramelc compile --dump-ast fib.erl
