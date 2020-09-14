% Source code generated with Caramel.
-module(calls).


-export([add/1]).
-export([add_twice/1]).
-export([call_nested/1]).
-export([call_other/1]).
-export([call_other_nested/1]).
-export([double/2]).

add(X) -> X.

double(F, X) -> F(F(X)).

add_twice(X) -> double(fun add/1, X).

call_nested(X) -> calls__nested:f(X, {}).

call_other(X) -> call_other:f(X).

call_other_nested(X) -> call_other__nested:f(X).


