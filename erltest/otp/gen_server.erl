% Source code generated with Caramel.
-module(gen_server).


-export([start/1]).
-export([start_link/3]).

start({}) -> dummy:start({}).

start_link(Mod_name, Init_arg, Opts) -> {ok, process:spawn(fun start/1)}.


