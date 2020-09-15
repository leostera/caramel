% Source code generated with Caramel.
-module(list_rev_sort).
-export_type([t/1]).

-export([length/1]).
-export([length_aux/2]).
-export([rev_append/2]).
-export([sort_uniq/2]).

-type t(A) :: []
            | {::, A, list(A)}
            .

length_aux(Len) -> fun([]) -> Len;
anonymous([_ | L]) -> length_aux(+(Len, 1), L)
 end.

length(L) -> length_aux(0, L).

rev_append(L1, L2) ->
  case L1 of
    [] -> L2;
    [A | L] -> rev_append(L, [A | L2])
  end.

sort_uniq(Cmp, L) ->
  Rev_merge = fun(L1, L2, Accu) ->
  case {L1, L2} of
    {[], L2} -> rev_append(L2, Accu);
    {L1, []} -> rev_append(L1, Accu);
    {[H1 | T1], [H2 | T2]} -> C = Cmp(H1, H2),
case =(C, 0) of
  true -> rev_merge(T1, T2, [H1 | Accu]);
  false -> case <(C, 0) of
  true -> rev_merge(T1, L2, [H1 | Accu]);
  false -> rev_merge(L1, T2, [H2 | Accu])
end
end
  end
end,
  Rev_merge_rev = fun(L1, L2, Accu) ->
  case {L1, L2} of
    {[], L2} -> rev_append(L2, Accu);
    {L1, []} -> rev_append(L1, Accu);
    {[H1 | T1], [H2 | T2]} -> C = Cmp(H1, H2),
case =(C, 0) of
  true -> rev_merge_rev(T1, T2, [H1 | Accu]);
  false -> case >(C, 0) of
  true -> rev_merge_rev(T1, L2, [H1 | Accu]);
  false -> rev_merge_rev(L1, T2, [H2 | Accu])
end
end
  end
end,
  Sort = fun(N, L) ->
  case {N, L} of
    {_, [X1 | [X2 | Tl]]} -> S = C = Cmp(X1, X2),
case =(C, 0) of
  true -> [X1 | []];
  false -> case <(C, 0) of
  true -> [X1 | [X2 | []]];
  false -> [X2 | [X1 | []]]
end
end,
{S, Tl};
    {_, [X1 | [X2 | [X3 | Tl]]]} -> S = C = Cmp(X1, X2),
case =(C, 0) of
  true -> C = Cmp(X2, X3),
case =(C, 0) of
  true -> [X2 | []];
  false -> case <(C, 0) of
  true -> [X2 | [X3 | []]];
  false -> [X3 | [X2 | []]]
end
end;
  false -> case <(C, 0) of
  true -> C = Cmp(X2, X3),
case =(C, 0) of
  true -> [X1 | [X2 | []]];
  false -> case <(C, 0) of
  true -> [X1 | [X2 | [X3 | []]]];
  false -> C = Cmp(X1, X3),
case =(C, 0) of
  true -> [X1 | [X2 | []]];
  false -> case <(C, 0) of
  true -> [X1 | [X3 | [X2 | []]]];
  false -> [X3 | [X1 | [X2 | []]]]
end
end
end
end;
  false -> C = Cmp(X1, X3),
case =(C, 0) of
  true -> [X2 | [X1 | []]];
  false -> case <(C, 0) of
  true -> [X2 | [X1 | [X3 | []]]];
  false -> C = Cmp(X2, X3),
case =(C, 0) of
  true -> [X2 | [X1 | []]];
  false -> case <(C, 0) of
  true -> [X2 | [X3 | [X1 | []]]];
  false -> [X3 | [X2 | [X1 | []]]]
end
end
end
end
end
end,
{S, Tl};
    {N, L} -> N1 = asr(N, 1),
N2 = -(N, N1),
{S1, L2} = rev_sort(N1, L),
{S2, Tl} = rev_sort(N2, L2),
{Rev_merge_rev(S1, S2, []), Tl}
  end
end,
  Rev_sort = fun(N, L) ->
  case {N, L} of
    {_, [X1 | [X2 | Tl]]} -> S = C = Cmp(X1, X2),
case =(C, 0) of
  true -> [X1 | []];
  false -> case >(C, 0) of
  true -> [X1 | [X2 | []]];
  false -> [X2 | [X1 | []]]
end
end,
{S, Tl};
    {_, [X1 | [X2 | [X3 | Tl]]]} -> S = C = Cmp(X1, X2),
case =(C, 0) of
  true -> C = Cmp(X2, X3),
case =(C, 0) of
  true -> [X2 | []];
  false -> case >(C, 0) of
  true -> [X2 | [X3 | []]];
  false -> [X3 | [X2 | []]]
end
end;
  false -> case >(C, 0) of
  true -> C = Cmp(X2, X3),
case =(C, 0) of
  true -> [X1 | [X2 | []]];
  false -> case >(C, 0) of
  true -> [X1 | [X2 | [X3 | []]]];
  false -> C = Cmp(X1, X3),
case =(C, 0) of
  true -> [X1 | [X2 | []]];
  false -> case >(C, 0) of
  true -> [X1 | [X3 | [X2 | []]]];
  false -> [X3 | [X1 | [X2 | []]]]
end
end
end
end;
  false -> C = Cmp(X1, X3),
case =(C, 0) of
  true -> [X2 | [X1 | []]];
  false -> case >(C, 0) of
  true -> [X2 | [X1 | [X3 | []]]];
  false -> C = Cmp(X2, X3),
case =(C, 0) of
  true -> [X2 | [X1 | []]];
  false -> case >(C, 0) of
  true -> [X2 | [X3 | [X1 | []]]];
  false -> [X3 | [X2 | [X1 | []]]]
end
end
end
end
end
end,
{S, Tl};
    {N, L} -> N1 = asr(N, 1),
N2 = -(N, N1),
{S1, L2} = sort(N1, L),
{S2, Tl} = sort(N2, L2),
{Rev_merge(S1, S2, []), Tl}
  end
end,
  Len = length(L),
  case <(Len, 2) of
    true -> L;
    false -> fst(Sort(Len, L))
  end.


