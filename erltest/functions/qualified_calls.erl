% Source code generated with Caramel.
-module(qualified_calls).

-export([add/1]).
-export([add_twice/1]).
-export([call_nested/1]).
-export([call_other/1]).
-export([call_other_nested/1]).
-export([double/2]).

add(X) -> X.

double(F, X) -> F(F(X)).

add_twice(X) -> double(fun add/1, X).

call_nested(X) -> qualified_calls__nested:f(X, ok).

call_other(X) -> qualified_calls_helper:f(X).

call_other_nested(X) -> qualified_calls_helper__nested:f(X).


