% Source code generated with Caramel.
-module(match).
-export_type([int_pair/0]).

-export([match_atoms/0]).
-export([match_ignore/0]).
-export([match_int/0]).
-export([match_list/0]).
-export([match_record/0]).
-export([match_str/0]).
-export([match_tuples/0]).
-export([match_unit/0]).

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
    _ -> true
  end.

match_str() ->
  case <<"hello">> of
    _ -> true;
    _ -> true;
    _ -> true;
    _ -> true;
    _ -> true;
    _ -> true
  end.

match_record() ->
  case #{ fst => 0
 , snd => 1
 } of
    #{ fst := _, snd := _ } -> true;
    #{ fst := _ } -> true;
    #{ snd := _ } -> true
  end.

match_list() ->
  case [0 | [1 | []]] of
    [] -> true;
    [_ | _] -> true;
    [_ | []] -> true;
    [_ | [_ | _]] -> true;
    [_ | [_ | []]] -> true
  end.

match_tuples() ->
  case {1, true, <<"hello">>} of
    {_, _, _} -> true;
    {_, true, _} -> true;
    {_, true, _} -> true
  end.

match_atoms() ->
  case hello of
    joe -> true
  end.


