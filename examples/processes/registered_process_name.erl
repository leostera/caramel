% Source code generated with Caramel.
-module(registered_process_name).

-export([run/0]).

run() ->
  case registered_process_name__proc:start() of
    {ok, Pid} -> case registered_process_name__proc:where_is() of
  {some, Pid2} -> erlang:send(Pid, 1),
erlang:send(Pid2, 1)
end
  end.


