% Source code generated with Caramel.
-module(day_1).

-export([find_entry_pair/2]).
-export([find_entry_triplet/2]).
-export([run/0]).

-spec find_entry_triplet(integer(), list(integer())) -> option:t(integer()).
find_entry_triplet(Number, Entries) -> lists:foldl(fun
  (E1, Acc1) ->
  case Acc1 of
    none -> lists:foldl(fun
  (E2, Acc2) ->
  case Acc2 of
    none -> lists:foldl(fun
  (E3, Acc3) ->
  case Acc3 of
    none -> case erlang:'=:='(Number, erlang:'+'(erlang:'+'(E1, E2), E3)) of
  true -> {some, erlang:'*'(erlang:'*'(E1, E2), E3)};
  false -> none
end;
    _ -> Acc3
  end
end, none, Entries);
    _ -> Acc2
  end
end, none, Entries);
    _ -> Acc1
  end
end, none, Entries).

-spec find_entry_pair(integer(), list(integer())) -> option:t(integer()).
find_entry_pair(Number, Entries) -> lists:foldl(fun
  (E1, Acc1) ->
  case Acc1 of
    none -> lists:foldl(fun
  (E2, Acc2) ->
  case Acc2 of
    none -> case erlang:'=:='(Number, erlang:'+'(E1, E2)) of
  true -> {some, erlang:'*'(E1, E2)};
  false -> none
end;
    _ -> Acc2
  end
end, none, Entries);
    _ -> Acc1
  end
end, none, Entries).

-spec run() -> list(option:t(integer())).
run() -> [find_entry_pair(2020, [0 | [0 | [0 | []]]]) | [find_entry_triplet(2020, [0 | [0 | [0 | []]]]) | []]].


