% Source code generated with Caramel.
-module(names).

-export([run_local/1]).
-export([run_macros/1]).
-export([run_nested/1]).
-export([run_nested_ambiguous/1]).

run_local() ->
  X = fun run_local/1,
  Y = atom,
  Y.

run_macros() ->
  Z = ,
  Z.

run_nested() ->
  names__nested:x(),
  names__nested:w().

run_nested_ambiguous() ->
  X = fun
  () -> 1
end,
  X().


