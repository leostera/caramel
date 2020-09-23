% Source code generated with Caramel.
-module(ignored_arguments).

-export([fst/1]).
-export([left/2]).
-export([right/2]).
-export([snd/1]).

left(L, _) -> L.

right(_, R) -> R.

fst({A, _}) -> A.

snd({_, B}) -> B.


