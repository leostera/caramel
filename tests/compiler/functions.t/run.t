  $ ls *.ml *.mli
  basic.ml
  hello_joe.ml
  ignored_arguments.ml
  io.ml
  labeled_arguments.ml
  multiple_clauses.ml
  partial_functions.ml
  qualified_calls.ml
  qualified_calls_helper.ml
  sequencing.ml
  uncurry.ml
  uncurry.mli
  $ caramelc compile *.ml *.mli
  File "partial_functions.ml", line 1, characters 9-21:
  1 | let head (x :: _) = x
               ^^^^^^^^^^^^
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  File "partial_functions.ml", line 3, characters 9-23:
  3 | let tail (_ :: xs) = xs
               ^^^^^^^^^^^^^^
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  File "partial_functions.ml", line 5, characters 11-20:
  5 | let one_el [ x ] = x
                 ^^^^^^^^^
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  (_::_::_|[])
  File "partial_functions.ml", line 7, characters 9-26:
  7 | let at_2 (_ :: x :: _) = x
               ^^^^^^^^^^^^^^^^^
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  (_::[]|[])
  File "partial_functions.ml", line 9, characters 9-31:
  9 | let at_3 (_ :: _ :: x :: _) = x
               ^^^^^^^^^^^^^^^^^^^^^^
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  (_::_::[]|_::[]|[])
  File "multiple_clauses.ml", line 1, characters 22-34:
  1 | let iff_using_headers true f _ = f
                            ^^^^^^^^^^^^
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  false
  Compiling uncurry.erl	OK
  Compiling qualified_calls_helper__nested.erl	OK
  Compiling qualified_calls_helper.erl	OK
  Compiling qualified_calls__nested.erl	OK
  Compiling qualified_calls.erl	OK
  Compiling partial_functions.erl	OK
  Compiling multiple_clauses.erl	OK
  Compiling labeled_arguments.erl	OK
  Compiling ignored_arguments.erl	OK
  Compiling hello_joe.erl	OK
  Compiling basic.erl	OK
  Compiling sequencing.erl	OK
  $ cat *.erl
  % Source code generated with Caramel.
  -module(basic).
  
  -export([combine/2]).
  -export([ignore/0]).
  -export([pair/2]).
  
  pair(X, Y) -> {X, Y}.
  
  combine({A, B}, {C, D}) -> {{A, C}, {B, D}}.
  
  ignore() -> ok.
  
  
  % Source code generated with Caramel.
  -module(hello_joe).
  
  -export([hello/0]).
  
  hello() ->
    Text = <<"hello, joe!">>,
    io:format(<<"~p">>, [Text | []]).
  
  
  % Source code generated with Caramel.
  -module(ignored_arguments).
  
  -export([fst/1]).
  -export([left/2]).
  -export([right/2]).
  -export([snd/1]).
  
  left(L, _) -> L.
  
  right(_, R) -> R.
  
  fst({A, _}) -> A.
  
  snd({_, B}) -> B.
  
  
  % Source code generated with Caramel.
  -module(labeled_arguments).
  
  -export([concat/2]).
  -export([run/0]).
  
  concat(A, B) -> << A/binary, B/binary >>.
  
  run() ->
    S1 = concat(<<"ocaml">>, <<"erlang">>),
    S2 = concat(<<"erlang">>, <<"ocaml">>),
    erlang:'=:='(S1, S2).
  
  
  % Source code generated with Caramel.
  -module(multiple_clauses).
  
  -export([iff_using_function/1]).
  -export([iff_using_headers/3]).
  -export([iff_using_if/3]).
  -export([iff_using_match/3]).
  
  iff_using_headers(true, F, _) -> F.
  
  iff_using_function({false, _, F}) -> F;
  iff_using_function({true, F, _}) -> F.
  
  iff_using_if(Cond, T, F) ->
    case Cond of
      true -> T();
      false -> F()
    end.
  
  iff_using_match(T, F, G) ->
    case T of
      true -> F;
      false -> G
    end.
  
  
  % Source code generated with Caramel.
  -module(partial_functions).
  
  -export([at_2/1]).
  -export([at_3/1]).
  -export([head/1]).
  -export([one_el/1]).
  -export([tail/1]).
  
  head([X | _]) -> X.
  
  tail([_ | Xs]) -> Xs.
  
  one_el([X | []]) -> X.
  
  at_2([_ | [X | _]]) -> X.
  
  at_3([_ | [_ | [X | _]]]) -> X.
  
  
  % Source code generated with Caramel.
  -module(qualified_calls).
  
  -export([add/1]).
  -export([add_twice/1]).
  -export([call_nested/1]).
  -export([call_other/1]).
  -export([call_other_nested/1]).
  -export([double/2]).
  
  add(X) -> X.
  
  double(F, X) -> F(F(X)).
  
  add_twice(X) -> double(fun add/1, X).
  
  call_nested(X) -> qualified_calls__nested:f(X).
  
  call_other(X) -> qualified_calls_helper:f(X).
  
  call_other_nested(X) -> qualified_calls_helper__nested:f(X).
  
  
  % Source code generated with Caramel.
  -module(qualified_calls__nested).
  
  -export([f/1]).
  
  f(X) ->
    case X of
      true -> ok;
      false -> ok
    end.
  
  
  % Source code generated with Caramel.
  -module(qualified_calls_helper).
  
  -export([f/1]).
  
  f(_X) -> ok.
  
  
  % Source code generated with Caramel.
  -module(qualified_calls_helper__nested).
  
  -export([f/1]).
  
  f(_X) -> ok.
  
  
  % Source code generated with Caramel.
  -module(sequencing).
  
  -export([run/0]).
  
  run() ->
    A = 1,
    B = 2,
    io:format(<<"Hello there\n">>, []),
    io:format(<<"Today we are adding ~p and ~p\n">>, [A | [B | []]]),
    io:format(<<"Here we go: ~p\n">>, [erlang:'+'(A, B) | []]),
    io:format(<<"*micdrop*">>, []).
  
  
  % Source code generated with Caramel.
  -module(uncurry).
  -export_type([defer/1]).
  -export_type([ignore/0]).
  
  -export([add/2]).
  -export([add_really_slow/1]).
  -export([add_slow/2]).
  -export([ignore/1]).
  
  -type ignore() :: fun((ok) -> ok).
  
  -type defer(A) :: fun((ok) -> A).
  
  ignore(_X, ok) -> ok.
  
  add(X, Y) -> erlang:'+'(X, X).
  
  add_slow(X, Y, ok) -> erlang:'+'(X, Y).
  
  add_really_slow(X, ok, Y, ok) -> erlang:'+'(X, Y).
  
  
