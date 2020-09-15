% Source code generated with Caramel.
-module(match).


-export([iff/3]).
-export([is_empty/1]).

iff(Cond, T, F) ->
  case Cond of
    true -> T({});
    false -> F({})
  end.

is_empty(L) ->
  case L of
    [] -> true;
    [_ | _] -> false
  end.


