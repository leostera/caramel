% Source code generated with Caramel.
-module(basic).


-export([fst/1]).
-export([left/2]).
-export([pair/2]).
-export([right/2]).
-export([snd/1]).

pair(X, Y) -> {X, Y}.

left(L, _) -> L.

right(_, R) -> R.

fst({A, _}) -> A.

snd({_, B}) -> B.


