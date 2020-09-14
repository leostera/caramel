% Source code generated with Caramel.
-module(typed_values).


-export([cons/2]).
-export([err/1]).
-export([nil/1]).
-export([none/1]).
-export([ok/1]).
-export([poly/1]).
-export([some/1]).

ok(X) -> {ok, X}.

err(X) -> {error, X}.

nil({}) -> nil.

cons(X, Xs) -> {cons, {X, Xs}}.

some(X) -> {some, X}.

none({}) -> none.

poly(X) -> {poly, X}.


