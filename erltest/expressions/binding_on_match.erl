% Source code generated with Caramel.
-module(binding_on_match).
-export_type([int_pair/0]).

-export([match_atoms/0]).
-export([match_list/0]).
-export([match_record/0]).
-export([match_tuples/0]).

-type int_pair() :: #{ fst => integer()
                     , snd => integer()
                     }.

match_record() ->
  case #{ fst => 0
 , snd => 1
 } of
    #{ fst := Fst, snd := Snd } -> true;
    #{ fst := X } -> true
  end.

match_list() ->
  case [0 | [1 | []]] of
    [] -> true;
    [X | []] -> true;
    [X | Xs] -> true
  end.

match_tuples() ->
  case {1, true, <<"hello">>} of
    {X, Y, Z} -> true;
    {X, _, _} -> true;
    X -> true
  end.

match_atoms() ->
  case hello of
    X -> true
  end.


