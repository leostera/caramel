% Source code generated with Caramel.
-module(proc_registry).

-export([register/2]).
-export([where_is/1]).

-spec where_is(A) :: option:t(erlang:pid(Message)).
where_is(Name) ->
  Pid = erlang:whereis(Name),
  Is_pid = erlang:is_pid(Pid),
  case Is_pid of
    true -> {some, Pid};
    _ -> none
  end.

-spec register(A, erlang:pid(Message)) :: result:t(erlang:pid(Message), binary()).
register(Name, Pid) ->
  case where_is(Name) of
    {some, _} -> {error, <<"process already registered">>};
    none -> case erlang:register(Name, Pid) of
  true -> {ok, Pid};
  false -> {error, <<"could not register process">>}
end
  end.


