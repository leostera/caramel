% Source code generated with Caramel.
-module(let_bindings).

-export([let_ignore/0]).
-export([let_many/0]).
-export([let_nested/3]).
-export([let_one/0]).
-export([let_rec/0]).

let_one() ->
  A = 1,
  A.

let_ignore() ->
  _ = 1,
  2.

let_many() ->
  A = 1,
  B = 2,
  C = 3,
  D = 4,
  erlang:'+'(erlang:'+'(erlang:'+'(A, B), C), D).

let_nested(F, G, H) ->
  A = _ = G(),
B = _ = H(),
C = 1,
erlang:'+'(C, 1),
erlang:'+'(B, 1),
  F(A).

let_rec() ->
  F = fun
  (X) -> f(erlang:'+'(X, 1))
end,
  F(0).


