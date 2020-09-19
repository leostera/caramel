% Source code generated with Caramel.
-module(basic).

-export([combine/2]).
-export([ignore/1]).
-export([pair/2]).

pair(X, Y) -> {X, Y}.

combine({A, B}, {C, D}) -> {{A, C}, {B, D}}.

ignore() -> ok.


