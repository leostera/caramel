% Source code generated with Caramel.
-module(fn).

-export_type([add/0]).
-export_type([f/2]).
-export_type([predicate/1]).

-type predicate(A) :: fun((A) -> bool()).

-type add() :: fun((int(), int()) -> int()).

-type f(A, B) :: fun((A, B) -> bool()).


