% Source code generated with Caramel.
-module(list).
-export_type([t/1]).

-export([append/2]).
-export([assoc/2]).
-export([assoc_opt/2]).
-export([assq/2]).
-export([assq_opt/2]).
-export([combine/2]).
-export([compare_length_with/2]).
-export([compare_lengths/2]).
-export([concat/1]).
-export([concat_map/2]).
-export([cons/2]).
-export([exists/2]).
-export([exists2/3]).
-export([filter/2]).
-export([filter_map/2]).
-export([filteri/2]).
-export([find/2]).
-export([find_all/2]).
-export([find_map/2]).
-export([find_opt/2]).
-export([flatten/1]).
-export([fold_left/3]).
-export([fold_left2/4]).
-export([fold_left_map/3]).
-export([fold_right/3]).
-export([fold_right2/4]).
-export([for_all/2]).
-export([for_all2/3]).
-export([hd/1]).
-export([init/2]).
-export([init_aux/3]).
-export([init_tailrec_aux/4]).
-export([iter/2]).
-export([iter2/3]).
-export([iteri/2]).
-export([iteri/3]).
-export([length/1]).
-export([length_aux/2]).
-export([map/2]).
-export([map2/3]).
-export([mapi/2]).
-export([mapi/3]).
-export([mem/2]).
-export([mem_assoc/2]).
-export([mem_assq/2]).
-export([memq/2]).
-export([merge/3]).
-export([nth/2]).
-export([nth_opt/2]).
-export([of_seq/1]).
-export([partition/2]).
-export([remove_assoc/2]).
-export([remove_assq/2]).
-export([rev/1]).
-export([rev_append/2]).
-export([rev_init_threshold/1]).
-export([rev_map/2]).
-export([rev_map2/3]).
-export([split/1]).
-export([tl/1]).
-export([to_seq/1]).

-type t(A) :: []
            | {::, A, list(A)}
            .

length_aux(Len) -> fun
   ([]) -> Len;
   ([_ | L]) -> length_aux(erlang:'+'(Len, 1), L)
 end.

length(L) -> length_aux(0, L).

cons(A, L) -> [A | L].

hd([]) -> failwith(<<"hd">>);
hd([A | _]) -> A.

tl([]) -> failwith(<<"tl">>);
tl([_ | L]) -> L.

nth(L, N) ->
  case erlang:'<'(N, 0) of
    true -> invalid_arg(<<"List.nth">>);
    false -> Nth_aux = fun
  (L, N) ->
  case L of
    [] -> failwith(<<"nth">>);
    [A | L] -> case erlang:'=:='(N, 0) of
  true -> A;
  false -> nth_aux(L, erlang:'-'(N, 1))
end
  end
end,
Nth_aux(L, N)
  end.

nth_opt(L, N) ->
  case erlang:'<'(N, 0) of
    true -> invalid_arg(<<"List.nth">>);
    false -> Nth_aux = fun
  (L, N) ->
  case L of
    [] -> none;
    [A | L] -> case erlang:'=:='(N, 0) of
  true -> {some, A};
  false -> nth_aux(L, erlang:'-'(N, 1))
end
  end
end,
Nth_aux(L, N)
  end.

rev_append(L1, L2) ->
  case L1 of
    [] -> L2;
    [A | L] -> rev_append(L, [A | L2])
  end.

rev(L) -> rev_append(L, []).

init_tailrec_aux(Acc, I, N, F) ->
  case >=(I, N) of
    true -> Acc;
    false -> init_tailrec_aux([F(I) | Acc], erlang:'+'(I, 1), N, F)
  end.

init_aux(I, N, F) ->
  case >=(I, N) of
    true -> [];
    false -> R = F(I),
[R | init_aux(erlang:'+'(I, 1), N, F)]
  end.

rev_init_threshold() ->
  case sys:backend_type() of
    _ -> 10000;
    {other, _} -> 50
  end.

init(Len, F) ->
  case erlang:'<'(Len, 0) of
    true -> invalid_arg(<<"List.init">>);
    false -> case erlang:'>'(Len, rev_init_threshold()) of
  true -> rev(init_tailrec_aux([], 0, Len, F));
  false -> init_aux(0, Len, F)
end
  end.

flatten([]) -> [];
flatten([L | R]) -> @(L, flatten(R)).

map(F) -> fun
   ([]) -> [];
   ([A | L]) ->
  R = F(A),
  [R | map(F, L)]
 end.

mapi(I, F) -> fun
   ([]) -> [];
   ([A | L]) ->
  R = F(I, A),
  [R | mapi(erlang:'+'(I, 1), F, L)]
 end.

mapi(F, L) -> mapi(0, F, L).

rev_map(F, L) ->
  Rmap_f = fun
  (Accu) -> fun
   ([]) -> Accu;
   ([A | L]) -> rmap_f([F(A) | Accu], L)
 end
end,
  Rmap_f([], L).

iter(F) -> fun
   ([]) -> ok;
   ([A | L]) ->
  F(A),
  iter(F, L)
 end.

iteri(I, F) -> fun
   ([]) -> ok;
   ([A | L]) ->
  F(I, A),
  iteri(erlang:'+'(I, 1), F, L)
 end.

iteri(F, L) -> iteri(0, F, L).

fold_left(F, Accu, L) ->
  case L of
    [] -> Accu;
    [A | L] -> fold_left(F, F(Accu, A), L)
  end.

fold_right(F, L, Accu) ->
  case L of
    [] -> Accu;
    [A | L] -> F(A, fold_right(F, L, Accu))
  end.

map2(F, L1, L2) ->
  case {L1, L2} of
    {[], []} -> [];
    {[A1 | L1], [A2 | L2]} -> R = F(A1, A2),
[R | map2(F, L1, L2)];
    {_, _} -> invalid_arg(<<"List.map2">>)
  end.

rev_map2(F, L1, L2) ->
  Rmap2_f = fun
  (Accu, L1, L2) ->
  case {L1, L2} of
    {[], []} -> Accu;
    {[A1 | L1], [A2 | L2]} -> rmap2_f([F(A1, A2) | Accu], L1, L2);
    {_, _} -> invalid_arg(<<"List.rev_map2">>)
  end
end,
  Rmap2_f([], L1, L2).

iter2(F, L1, L2) ->
  case {L1, L2} of
    {[], []} -> ok;
    {[A1 | L1], [A2 | L2]} -> F(A1, A2),
iter2(F, L1, L2);
    {_, _} -> invalid_arg(<<"List.iter2">>)
  end.

fold_left2(F, Accu, L1, L2) ->
  case {L1, L2} of
    {[], []} -> Accu;
    {[A1 | L1], [A2 | L2]} -> fold_left2(F, F(Accu, A1, A2), L1, L2);
    {_, _} -> invalid_arg(<<"List.fold_left2">>)
  end.

fold_right2(F, L1, L2, Accu) ->
  case {L1, L2} of
    {[], []} -> Accu;
    {[A1 | L1], [A2 | L2]} -> F(A1, A2, fold_right2(F, L1, L2, Accu));
    {_, _} -> invalid_arg(<<"List.fold_right2">>)
  end.

for_all(P) -> fun
   ([]) -> true;
   ([A | L]) -> &&(P(A), for_all(P, L))
 end.

exists(P) -> fun
   ([]) -> false;
   ([A | L]) -> ||(P(A), exists(P, L))
 end.

for_all2(P, L1, L2) ->
  case {L1, L2} of
    {[], []} -> true;
    {[A1 | L1], [A2 | L2]} -> &&(P(A1, A2), for_all2(P, L1, L2));
    {_, _} -> invalid_arg(<<"List.for_all2">>)
  end.

exists2(P, L1, L2) ->
  case {L1, L2} of
    {[], []} -> false;
    {[A1 | L1], [A2 | L2]} -> ||(P(A1, A2), exists2(P, L1, L2));
    {_, _} -> invalid_arg(<<"List.exists2">>)
  end.

mem(X) -> fun
   ([]) -> false;
   ([A | L]) -> ||(erlang:'=:='(compare(A, X), 0), mem(X, L))
 end.

memq(X) -> fun
   ([]) -> false;
   ([A | L]) -> ||(erlang:'=='(A, X), memq(X, L))
 end.

assoc(X) -> fun
   ([]) -> raise(not_found);
   ([{A, B} | L]) ->
  case erlang:'=:='(compare(A, X), 0) of
    true -> B;
    false -> assoc(X, L)
  end
 end.

assoc_opt(X) -> fun
   ([]) -> none;
   ([{A, B} | L]) ->
  case erlang:'=:='(compare(A, X), 0) of
    true -> {some, B};
    false -> assoc_opt(X, L)
  end
 end.

assq(X) -> fun
   ([]) -> raise(not_found);
   ([{A, B} | L]) ->
  case erlang:'=='(A, X) of
    true -> B;
    false -> assq(X, L)
  end
 end.

assq_opt(X) -> fun
   ([]) -> none;
   ([{A, B} | L]) ->
  case erlang:'=='(A, X) of
    true -> {some, B};
    false -> assq_opt(X, L)
  end
 end.

mem_assoc(X) -> fun
   ([]) -> false;
   ([{A, _} | L]) -> ||(erlang:'=:='(compare(A, X), 0), mem_assoc(X, L))
 end.

mem_assq(X) -> fun
   ([]) -> false;
   ([{A, _} | L]) -> ||(erlang:'=='(A, X), mem_assq(X, L))
 end.

remove_assoc(X) -> fun
   ([]) -> [];
   ([_ | L]) ->
  case erlang:'=:='(compare(, X), 0) of
    true -> L;
    false -> [ | remove_assoc(X, L)]
  end
 end.

remove_assq(X) -> fun
   ([]) -> [];
   ([_ | L]) ->
  case erlang:'=='(, X) of
    true -> L;
    false -> [ | remove_assq(X, L)]
  end
 end.

find(P) -> fun
   ([]) -> raise(not_found);
   ([X | L]) ->
  case P(X) of
    true -> X;
    false -> find(P, L)
  end
 end.

find_opt(P) -> fun
   ([]) -> none;
   ([X | L]) ->
  case P(X) of
    true -> {some, X};
    false -> find_opt(P, L)
  end
 end.

find_map(F) -> fun
   ([]) -> none;
   ([X | L]) ->
  case F(X) of
    _ -> ;
    none -> find_map(F, L)
  end
 end.

find_all(P) ->
  Find = fun
  (Accu) -> fun
   ([]) -> rev(Accu);
   ([X | L]) ->
  case P(X) of
    true -> find([X | Accu], L);
    false -> find(Accu, L)
  end
 end
end,
  Find([]).

filteri(P, L) ->
  Aux = fun
  (I, Acc) -> fun
   ([]) -> rev(Acc);
   ([X | L]) -> aux(erlang:'+'(I, 1), case P(I, X) of
  true -> [X | Acc];
  false -> Acc
end, L)
 end
end,
  Aux(0, [], L).

filter_map(F) ->
  Aux = fun
  (Accu) -> fun
   ([]) -> rev(Accu);
   ([X | L]) ->
  case F(X) of
    none -> aux(Accu, L);
    {some, V} -> aux([V | Accu], L)
  end
 end
end,
  Aux([]).

concat_map(F, L) ->
  Aux = fun
  (F, Acc) -> fun
   ([]) -> rev(Acc);
   ([X | L]) ->
  Xs = F(X),
  aux(F, rev_append(Xs, Acc), L)
 end
end,
  Aux(F, [], L).

fold_left_map(F, Accu, L) ->
  Aux = fun
  (Accu, L_accu) -> fun
   ([]) -> {Accu, rev(L_accu)};
   ([X | L]) ->
  {Accu, X} = F(Accu, X),
  aux(Accu, [X | L_accu], L)
 end
end,
  Aux(Accu, [], L).

partition(P, L) ->
  Part = fun
  (Yes, No) -> fun
   ([]) -> {rev(Yes), rev(No)};
   ([X | L]) ->
  case P(X) of
    true -> part([X | Yes], No, L);
    false -> part(Yes, [X | No], L)
  end
 end
end,
  Part([], [], L).

split([]) -> {[], []};
split([{X, Y} | L]) ->
  {Rx, Ry} = split(L),
  {[X | Rx], [Y | Ry]}.

combine(L1, L2) ->
  case {L1, L2} of
    {[], []} -> [];
    {[A1 | L1], [A2 | L2]} -> [{A1, A2} | combine(L1, L2)];
    {_, _} -> invalid_arg(<<"List.combine">>)
  end.

merge(Cmp, L1, L2) ->
  case {L1, L2} of
    {[], L2} -> L2;
    {L1, []} -> L1;
    {[H1 | T1], [H2 | T2]} -> case <=(Cmp(H1, H2), 0) of
  true -> [H1 | merge(Cmp, T1, L2)];
  false -> [H2 | merge(Cmp, L1, T2)]
end
  end.

compare_lengths(L1, L2) ->
  case {L1, L2} of
    {[], []} -> 0;
    {[], _} -> -1;
    {_, []} -> 1;
    {[_ | L1], [_ | L2]} -> compare_lengths(L1, L2)
  end.

compare_length_with(L, N) ->
  case L of
    [] -> case erlang:'=:='(N, 0) of
  true -> 0;
  false -> case erlang:'>'(N, 0) of
  true -> -1;
  false -> 1
end
end;
    [_ | L] -> case <=(N, 0) of
  true -> 1;
  false -> compare_length_with(L, erlang:'-'(N, 1))
end
  end.

to_seq(L) ->
  Aux = fun
  (L, ok) ->
  case L of
    [] -> nil;
    [X | Tail] -> {cons, X, aux(Tail)}
  end
end,
  Aux(L).

of_seq(Seq) ->
  Direct = fun
  (Depth, Seq) ->
  case erlang:'=:='(Depth, 0) of
    true -> |>(seq:fold_left(fun
  (Acc, X) -> [X | Acc]
end, [], Seq), fun rev/1);
    false -> case Seq() of
  nil -> [];
  {cons, X, Next} -> [X | direct(erlang:'-'(Depth, 1), Next)]
end
  end
end,
  Direct(500, Seq).


