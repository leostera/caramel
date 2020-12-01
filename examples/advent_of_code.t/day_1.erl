% Source code generated with Caramel.
-module(day_1).

-export([find_entries/2]).
-export([run/0]).

-spec find_entries(integer(), list(integer())) -> option:t(integer()).
find_entries(Number, Entries) -> lists:foldl(fun
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

-spec run() -> option:t(integer()).
run() -> find_entries(2020, [0 | [0 | [0 | []]]]).


