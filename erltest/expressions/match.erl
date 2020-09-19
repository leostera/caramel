% Source code generated with Caramel.
-module(match).
-export_type([int_pair/0]).

-export([match_atoms/1]).
-export([match_ignore/1]).
-export([match_int/1]).
-export([match_list/1]).
-export([match_record/1]).
-export([match_str/1]).
-export([match_tuples/1]).
-export([match_unit/1]).

-type int_pair() :: #{ fst => integer()
                     , snd => integer()
                     }.

match_unit() ->
  case ok of
    ok -> true
  end.

match_ignore() ->
  case ok of
    _ -> true
  end.

match_int() ->
  case 1 of
    1 -> true
  end.

match_str() ->
  case <<"hello">> of
    <<"xavier">> -> true;
    <<"remy">> -> true;
    <<"gal">> -> true;
    <<"mike">> -> true;
    <<"robert">> -> true;
    <<"joe">> -> true
  end.

match_record() ->
  case #{ fst => 0
 , snd => 1
 } of
    #{ fst := 10, snd := 10 } -> true;
    #{ fst := 0 } -> true;
    #{ snd := 1 } -> true
  end.

match_list() ->
  case [0 | [1 | []]] of
    [] -> true;
    [1 | Xs] -> true;
    [1 | []] -> true;
    [0 | [1 | _]] -> true;
    [0 | [1 | []]] -> true
  end.

match_tuples() ->
  case {1, true, <<"hello">>} of
    {1, _, _} -> true;
    {1, true, _} -> true;
    {1, true, <<"hello">>} -> true
  end.

match_atoms() ->
  case hello of
    xavier -> true;
    joe -> true;
    _ -> false
  end.


