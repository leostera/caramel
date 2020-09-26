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
-export([fold_left/3]).
-export([iter/2]).
-export([map/2]).
-export([return/1]).
-export([unfold/2]).

-type node(A) :: nil
               | {cons, A, t(A)}
               .

-type t(A) :: fun((ok) -> node(A)).

-spec empty() :: node(any()).
empty() -> nil.

-spec return(any(), ok) :: node(any()).
return(X, ok) -> {cons, X, fun empty/1}.

-spec cons(any(), t(any()), ok) :: node(any()).
cons(X, Next, ok) -> {cons, X, Next}.

-spec append(t(any()), fun(() -> node(any()))) :: t(any()).
append(Seq1, Seq2, ok) ->
  case Seq1() of
    nil -> Seq2();
    {cons, X, Next} -> {cons, X, append(Next, Seq2)}
  end.

-spec map(fun((any()) -> any()), t(any())) :: t(any()).
map(F, Seq, ok) ->
  case Seq() of
    nil -> nil;
    {cons, X, Next} -> {cons, F(X), map(F, Next)}
  end.

-spec filter_map(fun((any()) -> option:t(any())), t(any())) :: t(any()).
filter_map(F, Seq, ok) ->
  case Seq() of
    nil -> nil;
    {cons, X, Next} -> case F(X) of
  none -> filter_map(F, Next, ok);
  {some, Y} -> {cons, Y, filter_map(F, Next)}
end
  end.

-spec filter(fun((any()) -> boolean()), t(any())) :: t(any()).
filter(F, Seq, ok) ->
  case Seq() of
    nil -> nil;
    {cons, X, Next} -> case F(X) of
  true -> {cons, X, filter(F, Next)};
  false -> filter(F, Next, ok)
end
  end.

-spec flat_map_app(fun((any()) -> t(any())), t(any()), t(any())) :: t(any()).
flat_map_app(F, Seq, Tail, ok) ->
  case Seq() of
    nil -> flat_map(F, Tail, ok);
    {cons, X, Next} -> {cons, X, flat_map_app(F, Next, Tail)}
  end.

-spec flat_map(fun((any()) -> t(any())), t(any()), ok) :: node(any()).
flat_map(F, Seq, ok) ->
  case Seq() of
    nil -> nil;
    {cons, X, Next} -> flat_map_app(F, F(X), Next, ok)
  end.

-spec fold_left(fun((any(), any()) -> any()), any(), t(any())) :: any().
fold_left(F, Acc, Seq) ->
  Aux = fun
  (F, Acc, Seq) ->
  case Seq() of
    nil -> Acc;
    {cons, X, Next} -> Acc = F(Acc, X),
aux(F, Acc, Next)
  end
end,
  Aux(F, Acc, Seq).

-spec iter(fun((any()) -> any()), t(any())) :: ok.
iter(F, Seq) ->
  Aux = fun
  (Seq) ->
  case Seq() of
    nil -> ok;
    {cons, X, Next} -> F(X),
aux(Next)
  end
end,
  Aux(Seq).

-spec unfold(fun((any()) -> option:t({any(), any()})), any()) :: t(any()).
unfold(F, U, ok) ->
  case F(U) of
    none -> nil;
    {some, {X, U'}} -> {cons, X, unfold(F, U')}
  end.


