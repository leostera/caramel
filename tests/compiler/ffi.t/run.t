  $ ls *.ml
  using_core.ml
  using_io.ml
  $ caramel compile *.ml
  Compiling using_io.erl	OK
  Compiling using_core.erl	OK
  $ echo $?
  0
  $ cat *.erl
  % Source code generated with Caramel.
  -module(using_core).
  
  -export([main/1]).
  -export([print/1]).
  
  -spec print(_) -> ok.
  print(Thing) -> io:format(<<"~0tp~n">>, [Thing | []]).
  
  -spec main(_) -> ok.
  main(_) ->
    print(erlang:'+'(1, 1)),
    print(erlang:'-'(1, 1)),
    print(erlang:'*'(1, 1)),
    print(erlang:'div'(1, 1)),
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
    io:format(<<"~p">>, [Str | []]),
    Ints = 1,
    io:format(<<"~p">>, [Ints | []]).
  
  
