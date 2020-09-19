% Source code generated with Caramel.
-module(hello_joe).

-export([hello/1]).

hello() ->
  Text = <<"hello, joe!">>,
  io:format(<<"~p">>, [Text | []]).


