% Source code generated with Caramel.
-module(basic).


-export([at_2/1]).
-export([at_3/1]).
-export([combine/2]).
-export([empty_list/1]).
-export([fst/1]).
-export([head/1]).
-export([iff/3]).
-export([iff2/1]).
-export([ignore/1]).
-export([left/2]).
-export([one_el/1]).
-export([pair/2]).
-export([right/2]).
-export([snd/1]).
-export([tail/1]).

pair(X, Y) -> {X, Y}.

left(L, _) -> L.

right(_, R) -> R.

fst({A, _}) -> A.

snd({_, B}) -> B.

combine({A, B}, {C, D}) -> {{A, C}, {B, D}}.

head([X | _]) -> X.

tail([_ | Xs]) -> Xs.

one_el([X | []]) -> X.

at_2([_ | [X | _]]) -> X.

at_3([_ | [_ | [X | _]]]) -> X.

iff(true, F, _) -> F.

iff2({false, _, F}) -> F;
iff2({true, F, _}) -> F.

ignore(_X) -> {}.

empty_list(_X) -> [].


