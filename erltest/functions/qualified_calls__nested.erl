% Source code generated with Caramel.
-module(qualified_calls__nested).

-export([f/2]).

f(X, ok) ->
  case X of
    true -> ok;
    false -> ok
  end.


