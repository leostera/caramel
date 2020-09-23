% Source code generated with Caramel.
-module(adder).
-export_type([call_response/0]).
-export_type([init_args/0]).
-export_type([message/0]).
-export_type([reply/0]).
-export_type([state/0]).

-export([add/2]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([init/1]).
-export([start_link/1]).

-type init_args() :: integer().

-type message() :: {add, integer()}
                 .

-type reply() :: integer().

-type state() :: integer().

-type call_response() :: {reply, reply(), state()}
                       | {no_reply, state()}
                       .

init(X) -> {ok, X}.

handle_cast(_, State) -> {no_reply, State}.

handle_call(Message, _Pid, State) ->
  timer:sleep(1000),
  case Message of
    {add, I} -> {reply, State, erlang:'+'(State, I)}
  end.

add(Pid, X) -> {ok, gen_server:call(Pid, X)}.

start_link(Args) -> gen_server:start_link(adder, Args, []).


