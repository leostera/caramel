  $ caramel compile *.ml *.mli
  File "registered_process_name.ml", lines 30-32, characters 2-20:
  30 | ..let (Some pid2) = Proc.where_is () in
  31 |   Erlang.send pid 1;
  32 |   Erlang.send pid2 1
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  None
  File "registered_process_name.ml", lines 29-32, characters 2-20:
  29 | ..let (Ok pid) = Proc.start () in
  30 |   let (Some pid2) = Proc.where_is () in
  31 |   Erlang.send pid 1;
  32 |   Erlang.send pid2 1
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  Error _
  Compiling typed_reply.erl	OK
  Compiling tiny.erl	OK
  Compiling proc_registry.erl	OK
  Compiling looper.erl	OK
  Compiling holder_annotated.erl	OK
  Compiling holder.erl	OK
  Compiling registered_process_name__proc.erl	OK
  Compiling registered_process_name.erl	OK
  $ cat *.erl
  % Source code generated with Caramel.
  -module(holder).
  
  -export([do_work/0]).
  -export([handle_message/2]).
  -export([loop/2]).
  -export([start/1]).
  
  -spec handle_message({binary(), integer()}, option:t({add, integer()}
              | {hello, binary()}
              | reset
              )) -> {binary(), integer()}.
  handle_message(State, Msg) ->
    {X, Y} = State,
    case Msg of
      {some, reset} -> {<<"">>, 0};
      {some, {add, Z}} -> {X, Z};
      {some, {hello, N}} -> {N, Y};
      none -> State
    end.
  
  -spec loop(fun((beam__process:after_time()) -> option:t({add, integer()}
    | {hello, binary()}
    | reset
    )), {binary(), integer()}) -> _.
  loop(Recv, State) ->
    io:format(<<"current_state: ~p\n">>, [State | []]),
    Msg = Recv({bounded, 5000}),
    State2 = handle_message(State, Msg),
    loop(Recv, State2).
  
  -spec start({binary(), integer()}) -> erlang:pid().
  start(X) -> process:make(fun
    (_self, Recv) -> loop(Recv, X)
  end).
  
  -spec do_work() -> ok.
  do_work() ->
    Pid = start({<<"hi">>, 0}),
    erlang:send(Pid, {hello, <<"joe">>}).
  
  
  % Source code generated with Caramel.
  -module(holder_annotated).
  -export_type([msg/0]).
  -export_type([state/0]).
  
  -export([do_work/0]).
  -export([handle_message/2]).
  -export([loop/2]).
  -export([start/1]).
  
  -type msg() :: reset
               | {add, integer()}
               | {hello, binary()}
               .
  
  -type state() :: {binary(), integer()}.
  
  -spec handle_message(state(), option:t(msg())) -> state().
  handle_message(State, Msg) ->
    {X, Y} = State,
    case Msg of
      {some, reset} -> {<<"">>, 0};
      {some, {add, Z}} -> {X, Z};
      {some, {hello, N}} -> {N, Y};
      none -> State
    end.
  
  -spec loop(fun((beam__process:after_time()) -> option:t(msg())), state()) -> ok.
  loop(Recv, State) ->
    io:format(<<"current_state: ~p\n">>, [State | []]),
    Msg = Recv({bounded, 5000}),
    State2 = handle_message(State, Msg),
    loop(Recv, State2).
  
  -spec start(state()) -> erlang:pid().
  start(X) -> process:make(fun
    (_self, Recv) -> loop(Recv, X)
  end).
  
  -spec do_work() -> ok.
  do_work() ->
    Pid = start({<<"hi">>, 0}),
    erlang:send(Pid, {hello, <<"joe">>}).
  
  
  % Source code generated with Caramel.
  -module(looper).
  
  -export([do_work/0]).
  -export([loop/1]).
  -export([start/1]).
  
  -spec loop(_) -> _.
  loop(X) ->
    io:format(<<"~p\n">>, [erlang:self() | []]),
    loop(X).
  
  -spec start(_) -> beam__erlang:pid(_).
  start(X) -> erlang:spawn(fun
    () -> loop(X)
  end).
  
  -spec do_work() -> ok.
  do_work() ->
    Pid = start(1),
    erlang:send(Pid, 2112).
  
  
  % Source code generated with Caramel.
  -module(proc_registry).
  
  -export([register/2]).
  -export([where_is/1]).
  
  -spec where_is(_) -> option:t(beam__erlang:pid(_)).
  where_is(Name) ->
    Pid = erlang:whereis(Name),
    Is_pid = erlang:is_pid(Pid),
    case Is_pid of
      true -> {some, Pid};
      _ -> none
    end.
  
  -spec register(_, beam__erlang:pid(_message)) -> result:t(beam__erlang:pid(_message), binary()).
  register(Name, Pid) ->
    case where_is(Name) of
      {some, _} -> {error, <<"pid already registered">>};
      none -> case erlang:register(Name, Pid) of
    true -> {ok, Pid};
    false -> {error, <<"could not register pid">>}
  end
    end.
  
  
  % Source code generated with Caramel.
  -module(registered_process_name).
  
  -export([run/0]).
  
  -spec run() -> ok.
  run() ->
    case registered_process_name__proc:start() of
      {ok, Pid} -> case registered_process_name__proc:where_is() of
    {some, Pid2} -> erlang:send(Pid, 1),
  erlang:send(Pid2, 1)
  end
    end.
  
  
  % Source code generated with Caramel.
  -module(registered_process_name__proc).
  -export_type([t/0]).
  
  -export([a_loop/3]).
  -export([name/0]).
  -export([start/0]).
  -export([where_is/0]).
  
  -type t() :: erlang:pid().
  
  -spec name() -> worker_pool
    .
  name() -> worker_pool.
  
  -spec where_is() -> option:t(t()).
  where_is() -> proc_registry:where_is(name()).
  
  -spec a_loop(_, fun((beam__process:after_time()) -> option:t(integer())), integer()) -> _.
  a_loop(Pid, Recv, I) ->
    case Recv(infinity) of
      none -> a_loop(Pid, Recv, I);
      {some, J} -> io:format(<<"recv: ~p\n">>, [J | []]),
  a_loop(Pid, Recv, erlang:'+'(I, J))
    end.
  
  -spec start() -> result:t(beam__erlang:pid(integer()), binary()).
  start() ->
    Loop = fun
    (Pid, Recv) -> a_loop(Pid, Recv, 0)
  end,
    Pid = process:make(Loop),
    proc_registry:register(name(), Pid).
  
  
  % Source code generated with Caramel.
  -module(tiny).
  
  -export([loop/2]).
  -export([start/1]).
  
  -spec loop(integer(), _) -> _.
  loop(T, Recv) ->
    io:format(<<"~p\n">>, [T | []]),
    timer:sleep(T),
    loop(erlang:'*'(T, 2), Recv).
  
  -spec start(integer()) -> erlang:pid().
  start(T) -> process:make(fun
    (_self, R) -> loop(T, R)
  end).
  
  
  % Source code generated with Caramel.
  -module(typed_reply).
  
  -export([a_loop/3]).
  -export([b_loop/3]).
  -export([c_loop/3]).
  -export([run/0]).
  
  -spec a_loop(_, fun((beam__process:after_time()) -> option:t({call, {beam__erlang:pid(integer()), integer()}}
      | noop
      )), integer()) -> _.
  a_loop(Pid, Recv, I) ->
    case Recv(infinity) of
      none -> a_loop(Pid, Recv, I);
      {some, noop} -> a_loop(Pid, Recv, I);
      {some, {call, {Cb, Msg}}} -> erlang:send(Cb, I),
  a_loop(Pid, Recv, erlang:'+'(I, Msg))
    end.
  
  -spec b_loop(A, fun((beam__process:after_time()) -> option:t(integer())), beam__erlang:pid({call, {A, integer()}}
      )) -> ok.
  b_loop(Pid, Recv, A) ->
    timer:sleep(1000),
    erlang:send(A, {call, {Pid, 1}}),
    case Recv({bounded, 0}) of
      none -> b_loop(Pid, Recv, A);
      {some, 11} -> io:format(<<"aaadn i'm out\n">>, []);
      {some, I} -> io:format(<<"received: ~p, state must be ~p\n">>, [I | [erlang:'+'(I, 1) | []]]),
  b_loop(Pid, Recv, A)
    end.
  
  -spec c_loop(erlang:pid(), fun((beam__process:after_time()) -> option:t(boolean())), beam__erlang:pid({call, {erlang:pid(), integer()}}
      )) -> ok.
  c_loop(Pid, Recv, A) ->
    timer:sleep(1000),
    Cb = process:contramap(fun
    (X) -> erlang:'>'(X, 10)
  end, Pid),
    erlang:send(A, {call, {Cb, 1}}),
    case Recv({bounded, 0}) of
      none -> c_loop(Pid, Recv, A);
      {some, true} -> io:format(<<"yay, it is true\n">>, []);
      {some, false} -> io:format(<<"oh noes!\n">>, []),
  c_loop(Pid, Recv, A)
    end.
  
  -spec run() -> ok.
  run() ->
    A = process:make(fun
    (Pid, Recv) -> a_loop(Pid, Recv, 0)
  end),
    _b = process:make(fun
    (Pid, Recv) -> b_loop(Pid, Recv, A)
  end),
    _c = process:make(fun
    (Pid, Recv) -> c_loop(Pid, Recv, A)
  end),
    ok.
  
  
