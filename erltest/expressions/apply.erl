% Source code generated with Caramel.
-module(apply).

-export([f/1]).
-export([f1/1]).
-export([f2/2]).
-export([f3/3]).
-export([f4/4]).
-export([run/1]).

f(X) -> f(X).

f1(X) -> f([X | []]).

f2(X, Y) -> f([X | [Y | []]]).

f3(X, Y, Z) -> f([X | [Y | [Z | []]]]).

f4(W, X, Y, Z) -> f([W | [X | [Y | [Z | []]]]]).

run() ->
  f1(1),
  f2(1, 2),
  f3(1, 2, 3),
  f4(1, 2, 3, 4),
  apply__funs:apply_fun().


