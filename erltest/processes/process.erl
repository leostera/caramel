% Source code generated with Caramel.
-module(process).

-export([spawn/1]).

spawn(F) -> erlang:spawn(fun
  () -> F(fun (T) -> receive X -> {some, X} after T -> none end end)
end).


