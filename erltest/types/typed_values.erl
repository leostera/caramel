% Source code generated with Caramel.
-module(typed_values).


-export([cons/2]).
-export([err/1]).
-export([nil/1]).
-export([none/1]).
-export([ok/1]).
-export([some/1]).

ok(X) -> Ok.

err(X) -> Error.

nil({}) -> Nil.

cons(X, Xs) -> Cons.

some(X) -> Some.

none({}) -> None.


