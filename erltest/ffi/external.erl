% Source code generated with Caramel.
-module(external).

-export([fmt/1]).

fmt({}) ->
  Str = <<"Hello">>,
  io:format(<<"~p">>, [Str | []]).


