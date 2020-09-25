  $ ls *.ml
  io.ml
  using_io.ml
  $ caramelc compile *.ml
  Compiling using_io.erl	OK
  $ cat *.erl
  % Source code generated with Caramel.
  -module(using_io).
  
  -export([fmt/0]).
  
  -spec fmt() :: ok.
  fmt() ->
    Str = <<"Hello">>,
    io:format(<<"~p">>, [Str | []]),
    Ints = 1,
    io:format(<<"~p">>, [Ints | []]).
  
  
