% Source code generated with Caramel.
-module(adder).
-export_type([message/0]).

-export([add/2]).
-export([reset/1]).

-type message() :: {add, integer()}
                 | reset
                 .

-type t() :: integer().

-type resp() :: unit().

-type call_reply() :: {no_reply, t()}
                    | {reply, resp(), t()}
                    .

add(Pid, X) -> call(Pid, {add, X}).

reset(Pid) -> call(Pid, reset).


