% Source code generated with Caramel.
-module(registered_process_name__proc).
-export_type([t/0]).

-export([a_loop/3]).
-export([name/0]).
-export([start/0]).
-export([where_is/0]).

-type t() :: erlang:pid(integer()).

-spec name() :: worker_pool
  .
name() -> worker_pool.

-spec where_is() :: option:t(t()).
where_is() -> proc_registry:where_is(name()).

-spec a_loop(A, fun((process:after_time()) -> option:t(integer())), integer()) :: B.
a_loop(Pid, Recv, I) ->
  case Recv(infinity) of
    none -> a_loop(Pid, Recv, I);
    {some, J} -> io:format(<<"recv: ~p\n">>, [J | []]),
a_loop(Pid, Recv, erlang:'+'(I, J))
  end.

-spec start() :: result:t(erlang:pid(integer()), binary()).
start() ->
  Loop = fun
  (Pid, Recv) -> a_loop(Pid, Recv, 0)
end,
  Pid = process:make(Loop),
  proc_registry:register(name(), Pid).


