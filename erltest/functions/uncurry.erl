% Source code generated with Caramel.
-module(uncurry).
-export_type([defer/1]).
-export_type([ignore/0]).

-export([add/2]).
-export([add_really_slow/1]).
-export([add_slow/2]).
-export([ignore/1]).

-type ignore() :: fun((unit()) -> unit()).

-type defer(A) :: fun((unit()) -> A).

ignore(_X, ok) -> ok.

add(X, Y) -> erlang:'+'(X, X).

add_slow(X, Y, ok) -> erlang:'+'(X, Y).

add_really_slow(X, ok, Y, ok) -> erlang:'+'(X, Y).


