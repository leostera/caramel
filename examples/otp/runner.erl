% Source code generated with Caramel.
-module(runner).

-export([run/0]).

-spec run() :: ok.
run() ->
  Pid = adder:start_link(10),
  {ok, Reply} = adder:add(Pid, {add, 1}),
  io:format(<<"reply: ~p">>, [Reply | []]).


