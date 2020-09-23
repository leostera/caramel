% Source code generated with Caramel.
-module(using_io).

-export([fmt/0]).

fmt() ->
  Str = <<"Hello">>,
  io:format(<<"~p">>, [Str | []]),
  Ints = 1,
  io:format(<<"~p">>, [Ints | []]).


