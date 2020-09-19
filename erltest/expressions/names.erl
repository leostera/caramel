% Source code generated with Caramel.
-module(names).

-export([run_local/0]).
-export([run_macros/0]).
-export([run_nested/0]).
-export([run_nested_ambiguous/0]).

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


