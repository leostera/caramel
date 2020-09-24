-module(rogue_one).

-behavior(gen_server).
-behavior(test_beh).

-export([f/0]).

f() -> print_int(<<"hello">>).

g(1) -> ok;
g(0) -> rogue_one:f(err).
