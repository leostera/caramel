% Source code generated with Caramel.
-module(list).

-export([at_2/1]).
-export([concat/2]).
-export([cons/2]).
-export([empty/0]).
-export([head/1]).
-export([pair/1]).
-export([tail/1]).

empty() -> [].

pair(X) -> [X | [X | []]].

cons(X, Y) -> [X | Y].

head([X | _]) -> X.

tail([_ | X]) -> X.

at_2([_ | [X | _]]) -> X.

concat(A, B) -> @(A, B).


