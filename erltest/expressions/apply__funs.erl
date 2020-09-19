% Source code generated with Caramel.
-module(apply__funs).

-export([apply_fun/1]).

apply_fun() ->
  A = fun
  () -> 1
end,
  B = fun
  () -> 2
end,
  C = fun
  (X) -> erlang:'+'(X, 1)
end,
  A(),
  B(),
  C(0).


