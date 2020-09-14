% Source code generated with Caramel.
-module(calls).


-export([add/1]).
-export([add_twice/1]).
-export([double/2]).

add(X) -> X.

double(F, X) -> F(F(X)).

add_twice(X) -> double(fun add/1, X).


