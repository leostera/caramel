-module(rogue_one).

-author(abstract_machines).

-behavior(gen_server).
-behavior(test_beh).

-export_type([t/0]).
-export([f/0]).

f() -> print_int(<<"hello">>).

g(1) -> ok;
g(0) -> rogue_one:f(err).
