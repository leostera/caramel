% Source code generated with Caramel.
-module(adder).

-export_type([error/0]).
-export_type([message/0]).
-export_type([opts/0]).
-export_type([state/0]).

-export([init/1]).
-export([start_link/1]).

-type error() :: unit().

-type state() :: integer().

-type opts() :: unit().

-type message() :: {add, integer()}
                 | get
                 .

start_link({}) -> gen_server:start_link(?MODULE, [], none).

init({}) -> {ok, 0}.


