% Source code generated with Caramel.
-module(fn).

-export_type([add/0]).
-export_type([f/3]).
-export_type([predicate/1]).

-type predicate(A) :: fun(() -> A).

-type add() :: fun((int()) -> int()).

-type f(A, B, C) :: fun((A) -> B).


