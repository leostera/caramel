  $ ls *.ml
  apply.ml
  binding_on_match.ml
  funref.ml
  let_bindings.ml
  list.ml
  literals.ml
  match.ml
  names.ml
  records.ml
  $ caramelc compile *.ml
  File "names.ml", line 15, characters 2-13:
  15 |   Nested.x ();
         ^^^^^^^^^^^
  Warning 10: this expression should have type unit.
  File "names.ml", line 4, characters 6-7:
  4 |   let x = run_local in
            ^
  Warning 26: unused variable x.
  File "names.ml", line 21, characters 6-7:
  21 |   let x () = 1 in
             ^
  Warning 26: unused variable x.
  File "match.ml", line 5, characters 19-41:
  5 | let match_int () = match 1 with 1 -> true
                         ^^^^^^^^^^^^^^^^^^^^^^
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  0
  File "match.ml", lines 8-14, characters 2-17:
   8 | ..match "hello" with
   9 |   | "xavier" -> true
  10 |   | "remy" -> true
  11 |   | "gal" -> true
  12 |   | "mike" -> true
  13 |   | "robert" -> true
  14 |   | "joe" -> true
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  ""
  File "match.ml", lines 19-22, characters 2-23:
  19 | ..match { fst = 0; snd = 1 } with
  20 |   | { fst = 10; snd = 10 } -> true
  21 |   | { fst = 0 } -> true
  22 |   | { snd = 1 } -> true
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  ({fst=10; snd=0}|{fst=1; snd=0})
  File "match.ml", lines 25-30, characters 2-20:
  25 | ..match [ 0; 1 ] with
  26 |   | [] -> true
  27 |   | 1 :: xs -> true
  28 |   | [ 1 ] -> true
  29 |   | 0 :: 1 :: _ -> true
  30 |   | [ 0; 1 ] -> true
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  (0::0::_|0::[]|2::_)
  File "match.ml", line 28, characters 4-9:
  28 |   | [ 1 ] -> true
           ^^^^^
  Warning 11: this match case is unused.
  File "match.ml", line 30, characters 4-12:
  30 |   | [ 0; 1 ] -> true
           ^^^^^^^^
  Warning 11: this match case is unused.
  File "match.ml", lines 33-36, characters 2-28:
  33 | ..match (1, true, "hello") with
  34 |   | 1, _, _ -> true
  35 |   | 1, true, _ -> true
  36 |   | 1, true, "hello" -> true
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  (0, _, _)
  File "match.ml", line 35, characters 4-14:
  35 |   | 1, true, _ -> true
           ^^^^^^^^^^
  Warning 11: this match case is unused.
  File "match.ml", line 36, characters 4-20:
  36 |   | 1, true, "hello" -> true
           ^^^^^^^^^^^^^^^^
  Warning 11: this match case is unused.
  File "list.ml", line 7, characters 9-21:
  7 | let head (x :: _) = x
               ^^^^^^^^^^^^
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  File "list.ml", line 9, characters 9-21:
  9 | let tail (_ :: x) = x
               ^^^^^^^^^^^^
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  File "list.ml", line 11, characters 9-26:
  11 | let at_2 (_ :: x :: _) = x
                ^^^^^^^^^^^^^^^^^
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  (_::[]|[])
  File "binding_on_match.ml", line 4, characters 57-68:
  4 |   match { fst = 0; snd = 1 } with { fst; snd } -> true | { fst = x } -> true
                                                               ^^^^^^^^^^^
  Warning 11: this match case is unused.
  File "binding_on_match.ml", line 10, characters 50-57:
  10 |   match (1, true, "hello") with x, y, z -> true | x, _, _ -> true | x -> true
                                                         ^^^^^^^
  Warning 11: this match case is unused.
  File "binding_on_match.ml", line 10, characters 68-69:
  10 |   match (1, true, "hello") with x, y, z -> true | x, _, _ -> true | x -> true
                                                                           ^
  Warning 11: this match case is unused.
  File "apply.ml", line 6, characters 4-8:
  6 |     a ();
          ^^^^
  Warning 10: this expression should have type unit.
  File "apply.ml", line 7, characters 4-8:
  7 |     b ();
          ^^^^
  Warning 10: this expression should have type unit.
  File "apply.ml", line 22, characters 2-6:
  22 |   f1 1;
         ^^^^
  Warning 21: this statement never returns (or has an unsound type.)
  File "apply.ml", line 23, characters 2-8:
  23 |   f2 1 2;
         ^^^^^^
  Warning 21: this statement never returns (or has an unsound type.)
  File "apply.ml", line 24, characters 2-10:
  24 |   f3 1 2 3;
         ^^^^^^^^
  Warning 21: this statement never returns (or has an unsound type.)
  File "apply.ml", line 25, characters 2-12:
  25 |   f4 1 2 3 4;
         ^^^^^^^^^^
  Warning 21: this statement never returns (or has an unsound type.)
  Compiling records.erl	OK
  Compiling names__nested.erl	OK
  Compiling names.erl	OK
  Compiling match.erl	OK
  Compiling literals.erl	OK
  Compiling list.erl	OK
  Compiling let_bindings.erl	OK
  Compiling funref__nested.erl	OK
  Compiling funref.erl	OK
  Compiling binding_on_match.erl	OK
  Compiling apply__funs.erl	OK
  Compiling apply.erl	OK
  $ cat *.erl
  % Source code generated with Caramel.
  -module(apply).
  
  -export([f/1]).
  -export([f1/1]).
  -export([f2/2]).
  -export([f3/3]).
  -export([f4/4]).
  -export([run/0]).
  
  -spec f(any()) :: any().
  f(X) -> f(X).
  
  -spec f1(any()) :: any().
  f1(X) -> f([X | []]).
  
  -spec f2(any(), any()) :: any().
  f2(X, Y) -> f([X | [Y | []]]).
  
  -spec f3(any(), any(), any()) :: any().
  f3(X, Y, Z) -> f([X | [Y | [Z | []]]]).
  
  -spec f4(any(), any(), any(), any()) :: any().
  f4(W, X, Y, Z) -> f([W | [X | [Y | [Z | []]]]]).
  
  -spec run() :: integer().
  run() ->
    f1(1),
    f2(1, 2),
    f3(1, 2, 3),
    f4(1, 2, 3, 4),
    apply__funs:apply_fun().
  
  
  % Source code generated with Caramel.
  -module(apply__funs).
  
  -export([apply_fun/0]).
  
  -spec apply_fun() :: integer().
  apply_fun() ->
    A = fun
    () -> 1
  end,
    B = fun
    () -> 2
  end,
    C = fun
    (X) -> erlang:'+'(X, 1)
  end,
    A(),
    B(),
    C(0).
  
  
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
  
  -spec match_record() :: boolean().
  match_record() ->
    case #{ fst => 0
   , snd => 1
   } of
      #{ fst := Fst, snd := Snd } -> true;
      #{ fst := X } -> true
    end.
  
  -spec match_list() :: boolean().
  match_list() ->
    case [0 | [1 | []]] of
      '[]' -> true;
      [X | '[]'] -> true;
      [X | Xs] -> true
    end.
  
  -spec match_tuples() :: boolean().
  match_tuples() ->
    case {1, true, <<"hello">>} of
      {X, Y, Z} -> true;
      {X, _, _} -> true;
      X -> true
    end.
  
  -spec match_atoms() :: boolean().
  match_atoms() ->
    case hello of
      X -> true
    end.
  
  
  % Source code generated with Caramel.
  -module(funref).
  
  -export([add/2]).
  -export([call_op_2/3]).
  -export([do_add/2]).
  -export([do_nested_add/2]).
  
  -spec add(integer(), integer()) :: integer().
  add(X, Y) -> erlang:'+'(X, Y).
  
  -spec call_op_2(fun((any(), any()) -> any()), any(), any()) :: any().
  call_op_2(F, X, Y) -> F(X, Y).
  
  -spec do_add(integer(), integer()) :: integer().
  do_add(X, Y) -> call_op_2(fun add/2, X, Y).
  
  -spec do_nested_add(integer(), integer()) :: integer().
  do_nested_add(X, Y) -> call_op_2(funref__nested:add, X, Y).
  
  
  % Source code generated with Caramel.
  -module(funref__nested).
  
  -export([add/2]).
  
  -spec add(integer(), integer()) :: integer().
  add(X, Y) -> erlang:'+'(X, Y).
  
  
  % Source code generated with Caramel.
  -module(let_bindings).
  
  -export([let_ignore/0]).
  -export([let_many/0]).
  -export([let_nested/3]).
  -export([let_one/0]).
  -export([let_rec/0]).
  
  -spec let_one() :: integer().
  let_one() ->
    A = 1,
    A.
  
  -spec let_ignore() :: integer().
  let_ignore() ->
    1,
    2.
  
  -spec let_many() :: integer().
  let_many() ->
    A = 1,
    B = 2,
    C = 3,
    D = 4,
    erlang:'+'(erlang:'+'(erlang:'+'(A, B), C), D).
  
  -spec let_nested(fun((integer()) -> any()), fun(() -> any()), fun(() -> any())) :: any().
  let_nested(F, G, H) ->
    A = fun
    () ->
    G(),
    B = fun
    () ->
    H(),
    C = 1,
    erlang:'+'(C, 1)
  end(),
    erlang:'+'(B, 1)
  end(),
    F(A).
  
  -spec let_rec() :: any().
  let_rec() ->
    F = fun
    (X) -> f(erlang:'+'(X, 1))
  end,
    F(0).
  
  
  % Source code generated with Caramel.
  -module(list).
  
  -export([at_2/1]).
  -export([concat/2]).
  -export([cons/2]).
  -export([empty/0]).
  -export([head/1]).
  -export([pair/1]).
  -export([tail/1]).
  
  -spec empty() :: list(any()).
  empty() -> [].
  
  -spec pair(any()) :: list(any()).
  pair(X) -> [X | [X | []]].
  
  -spec cons(any(), list(any())) :: list(any()).
  cons(X, Y) -> [X | Y].
  
  -spec head(list(any())) :: any().
  head([X | _]) -> X.
  
  -spec tail(list(any())) :: list(any()).
  tail([_ | X]) -> X.
  
  -spec at_2(list(any())) :: any().
  at_2([_ | [X | _]]) -> X.
  
  -spec concat(list(any()), list(any())) :: list(any()).
  concat(A, B) -> erlang:'++'(A, B).
  
  
  % Source code generated with Caramel.
  -module(literals).
  
  -export([bool_false/0]).
  -export([bool_true/0]).
  -export([character/0]).
  -export([float/0]).
  -export([integer/0]).
  -export([string/0]).
  
  -spec integer() :: integer().
  integer() -> 1.
  
  -spec float() :: float().
  float() -> 1.0.
  
  -spec character() :: char().
  character() -> 'c'.
  
  -spec string() :: binary().
  string() -> <<"hello">>.
  
  -spec bool_true() :: boolean().
  bool_true() -> true.
  
  -spec bool_false() :: boolean().
  bool_false() -> true.
  
  
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
  
  -spec match_unit() :: boolean().
  match_unit() ->
    case ok of
      ok -> true
    end.
  
  -spec match_ignore() :: boolean().
  match_ignore() ->
    case ok of
      _ -> true
    end.
  
  -spec match_int() :: boolean().
  match_int() ->
    case 1 of
      1 -> true
    end.
  
  -spec match_str() :: boolean().
  match_str() ->
    case <<"hello">> of
      <<"xavier">> -> true;
      <<"remy">> -> true;
      <<"gal">> -> true;
      <<"mike">> -> true;
      <<"robert">> -> true;
      <<"joe">> -> true
    end.
  
  -spec match_record() :: boolean().
  match_record() ->
    case #{ fst => 0
   , snd => 1
   } of
      #{ fst := 10, snd := 10 } -> true;
      #{ fst := 0 } -> true;
      #{ snd := 1 } -> true
    end.
  
  -spec match_list() :: boolean().
  match_list() ->
    case [0 | [1 | []]] of
      '[]' -> true;
      [1 | Xs] -> true;
      [1 | '[]'] -> true;
      [0 | [1 | _]] -> true;
      [0 | [1 | '[]']] -> true
    end.
  
  -spec match_tuples() :: boolean().
  match_tuples() ->
    case {1, true, <<"hello">>} of
      {1, _, _} -> true;
      {1, true, _} -> true;
      {1, true, <<"hello">>} -> true
    end.
  
  -spec match_atoms() :: boolean().
  match_atoms() ->
    case hello of
      xavier -> true;
      joe -> true;
      _ -> false
    end.
  
  
  % Source code generated with Caramel.
  -module(names).
  
  -export([run_local/0]).
  -export([run_nested/0]).
  -export([run_nested_ambiguous/0]).
  
  -spec run_local() :: atom
         .
  run_local() ->
    X = fun run_local/1,
    Y = atom,
    Y.
  
  -spec run_nested() :: {version, kind_of_working
                  }
          .
  run_nested() ->
    names__nested:x(),
    names__nested:w().
  
  -spec run_nested_ambiguous() :: {compiler, binary()}
                    .
  run_nested_ambiguous() ->
    X = fun
    () -> 1
  end,
    X().
  
  
  % Source code generated with Caramel.
  -module(names__nested).
  
  -export([w/0]).
  -export([x/0]).
  
  -spec x() :: {compiler, binary()}
  .
  x() -> {compiler, <<"caramel">>}.
  
  -spec w() :: {version, kind_of_working
         }
  .
  w() -> {version, kind_of_working}.
  
  
  % Source code generated with Caramel.
  -module(records).
  -export_type([pair/1]).
  
  -export([flatten_first/1]).
  -export([fst/1]).
  -export([map/3]).
  -export([pair/2]).
  -export([snd/1]).
  -export([swap/1]).
  -export([swap_from_expr/2]).
  
  -type pair(A) :: #{ fst => A
                    , snd => A
                    }.
  
  -spec pair(any(), any()) :: pair(any()).
  pair(X, Y) ->
    #{ fst => X
     , snd => Y
     }.
  
  -spec fst(pair(any())) :: any().
  fst(#{ fst := Fst }) -> Fst.
  
  -spec snd(pair(any())) :: any().
  snd(#{ snd := Snd }) -> Snd.
  
  -spec swap(pair(any())) :: pair(any()).
  swap(P) ->
    #{ fst => maps:get(snd, P)
     , snd => maps:get(fst, P)
     }.
  
  -spec map(fun((any()) -> any()), fun((any()) -> any()), pair(any())) :: pair(any()).
  map(F, G, #{ fst := Fst, snd := Snd }) ->
    #{ fst => F(Fst)
     , snd => G(Snd)
     }.
  
  -spec swap_from_expr(any(), fun((any()) -> pair(any()))) :: pair(any()).
  swap_from_expr(P, F) ->
    #{ fst => maps:get(snd, F(P))
     , snd => maps:get(fst, F(P))
     }.
  
  -spec flatten_first(pair(pair(pair(any())))) :: pair(any()).
  flatten_first(P) ->
    #{ fst => maps:get(fst, maps:get(fst, maps:get(fst, P)))
     , snd => maps:get(snd, maps:get(fst, maps:get(fst, P)))
     }.
  
  
