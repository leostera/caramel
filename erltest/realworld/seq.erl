% Source code generated with Caramel.
-module(seq).

-export_type([node/1]).
-export_type([t/1]).

-export([append/2]).
-export([cons/2]).
-export([empty/0]).
-export([filter/2]).
-export([filter_map/2]).
-export([flat_map/2]).
-export([map/2]).
-export([return/1]).
-export([unfold/2]).

-type node(A) :: nil
               | {cons, A, t(A)}
               .

-type t(A) :: fun((unit()) -> node(A)).

empty({}) -> nil.

return(X, {}) -> {cons, X, fun empty/1}.

cons(X, Next, {}) -> {cons, X, Next}.

append(Seq1, Seq2, {}) ->
  case Seq1({}) of
    nil -> Seq2({});
    {cons, X, Next} -> {cons, X, append(Next, Seq2)}
  end.

map(F, Seq, {}) ->
  case Seq({}) of
    nil -> nil;
    {cons, X, Next} -> {cons, F(X), map(F, Next)}
  end.

filter_map(F, Seq, {}) ->
  case Seq({}) of
    nil -> nil;
    {cons, X, Next} -> case F(X) of
  none -> filter_map(F, Next, {});
  {some, Y} -> {cons, Y, filter_map(F, Next)}
end
  end.

filter(F, Seq, {}) ->
  case Seq({}) of
    nil -> nil;
    {cons, X, Next} -> case F(X) of
  true -> {cons, X, filter(F, Next)};
  _ -> filter(F, Next, {})
end
  end.

flat_map_app(F, Seq, Tail, {}) ->
  case Seq({}) of
    nil -> flat_map(F, Tail, {});
    {cons, X, Next} -> {cons, X, flat_map_app(F, Next, Tail)}
  end.

flat_map(F, Seq, {}) ->
  case Seq({}) of
    nil -> nil;
    {cons, X, Next} -> flat_map_app(F, F(X), Next, {})
  end.

unfold(F, U, {}) ->
  case F(U) of
    none -> nil;
    {some, {X, U2}} -> {cons, X, unfold(F, U2)}
  end.


