% Source code generated with Caramel.
-module(sequencing).

-export([run/0]).

run() ->
  A = 1,
  B = 2,
  _ = io:format(<<"Hello there\n">>, []),
  _ = io:format(<<"Today we are adding ~p and ~p\n">>, [A | [B | []]]),
  _ = io:format(<<"Here we go: ~p\n">>, [erlang:'+'(A, B) | []]),
  io:format(<<"*micdrop*">>, []).


