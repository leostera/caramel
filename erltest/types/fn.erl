% Source code generated with Caramel.
-module(fn).

-export_type([add/0]).
-export_type([f/2]).
-export_type([f_with_tuples/2]).
-export_type([nested/0]).
-export_type([predicate/1]).
-export_type([r/1]).


-type predicate(A) :: fun((A) -> boolean()).

-type add() :: fun((integer(), integer()) -> integer()).

-type f(A, B) :: fun((A, B) -> boolean()).

-type nested() :: fun((unit(), fun((integer()) -> boolean())) -> string()).

-type f_with_tuples(A, B) :: fun(({A, B}, unit()) -> boolean()).

-type r(A) :: #{ f => fun((unit()) -> {A, integer()}) }.


