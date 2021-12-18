  $ ls *.ml
  using_core.ml
  using_io.ml
  using_math.ml
  $ caramel compile --sugarcane *.ml
  Compiling using_core.core	OK
  Compiling using_io.core	OK
  Compiling using_math.core	OK
  $ echo $?
  0
  $ cat *.core
  % Source code generated with Caramel.
  -module(using_core).
  
  -export([main/1]).
  -export([print/1]).
  
  -spec print(_) -> ok.
  print(Thing) -> io:format(<<"~0tp~n">>, [Thing | []]).
  
  -spec main(_) -> ok.
  main(_) ->
    print({ok, 1}),
    print({error, 1}),
    print(erlang:'=:='(1, 1)),
    print(erlang:'=/='(1, 1)),
    print(erlang:'<'(1, 1)),
    print(erlang:'>'(1, 1)),
    print(erlang:'=<'(1, 1)),
    print(erlang:'>='(1, 1)),
    print(erlang:'=='(1, 1)),
    print(erlang:'=/='(1, 1)),
    print(erlang:'not'(true)),
    print(erlang:'and'(true, false)),
    print(erlang:'or'(true, false)),
    print(erlang:'++'([1 | []], [1 | []])),
    print(erlang:'-'(1)),
    print(erlang:'+'(1)),
    print(erlang:'+'(1, 1)),
    print(erlang:'-'(1, 1)),
    print(erlang:'*'(1, 1)),
    print(erlang:'div'(1, 1)),
    print(erlang:'rem'(1, 1)),
    print(erlang:'band'(1, 1)),
    print(erlang:'bor'(1, 1)),
    print(erlang:'bxor'(1, 1)),
    print(erlang:'bnot'(1)),
    print(erlang:'bsl'(-10, 1)),
    print(erlang:'bsr'(-10, 1)),
    print(erlang:'-'(1.0)),
    print(erlang:'+'(1.0)),
    print(erlang:'+'(1.0, 1.0)),
    print(erlang:'-'(1.0, 1.0)),
    print(erlang:'*'(1.0, 1.0)),
    print(erlang:'/'(1.0, 1.0)),
    ok.
  
  
  % Source code generated with Caramel.
  -module(using_io).
  
  -export([fmt/0]).
  
  -spec fmt() -> ok.
  fmt() ->
    Str = <<"Hello">>,
    begin
      io:format(<<"~p">>, [Str | []]),
      Ints = 1,
      io:format(<<"~p">>, [Ints | []])
    end.
  
  
  % Source code generated with Caramel.
  -module(using_math).
  
  -export([main/1]).
  -export([print/1]).
  
  -spec print(_) -> ok.
  print(Thing) -> io:format(<<"~0tp~n">>, [Thing | []]).
  
  -spec main(_) -> ok.
  main(_) ->
    print(math:sqrt(4.0)),
    print(math:exp(4.0)),
    print(math:log(4.0)),
    print(math:log10(4.0)),
    print(math:cos(4.0)),
    print(math:sin(4.0)),
    print(math:tan(4.0)),
    print(math:acos(1.0)),
    print(math:asin(1.0)),
    print(math:atan(4.0)),
    print(math:atan2(4.0, 4.0)),
    print(math:cosh(4.0)),
    print(math:sinh(4.0)),
    print(math:tanh(4.0)).
  
  
