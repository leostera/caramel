% Source code generated with Caramel.
-module(registered_process_name__proc).
-export_type([t/0]).

-export([a_loop/3]).
-export([name/0]).
-export([start/0]).
-export([where_is/0]).

-type t() :: erlang:pid().

name() -> worker_pool.

where_is() -> proc_registry:where_is(name()).

a_loop(Pid, Recv, I) ->
  case Recv(infinity) of
    none -> a_loop(Pid, Recv, I);
    {some, J} -> io:format(<<"recv: ~p\n">>, [J | []]),
a_loop(Pid, Recv, erlang:'+'(I, J))
  end.

start() ->
  Loop = fun
  (Pid, Recv) -> a_loop(Pid, Recv, 0)
end,
  Pid = process:make(Loop),
  proc_registry:register(name(), Pid).


