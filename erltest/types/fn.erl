% Source code generated with Caramel.
-module(fn).

-export_type([add/0]).
-export_type([f/2]).
-export_type([f_with_tuples/2]).
-export_type([nested/0]).
-export_type([predicate/1]).
-export_type([r/1]).

-type predicate(A) :: fun((A) -> bool()).

-type add() :: fun((int(), int()) -> int()).

-type f(A, B) :: fun((A, B) -> bool()).

-type nested() :: fun((unit(), fun((int()) -> bool())) -> string()).

-type f_with_tuples(A, B) :: fun(({A, B}, unit()) -> bool()).

-type r(A) :: #{ f => fun((unit()) -> {A, int()}) }.


