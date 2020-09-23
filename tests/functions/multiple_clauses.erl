% Source code generated with Caramel.
-module(multiple_clauses).

-export([iff_using_function/1]).
-export([iff_using_headers/3]).
-export([iff_using_if/3]).
-export([iff_using_match/3]).

iff_using_headers(true, F, _) -> F.

iff_using_function({false, _, F}) -> F;
iff_using_function({true, F, _}) -> F.

iff_using_if(Cond, T, F) ->
  case Cond of
    true -> T();
    false -> F()
  end.

iff_using_match(T, F, G) ->
  case T of
    true -> F;
    false -> G
  end.


