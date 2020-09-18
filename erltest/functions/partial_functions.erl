% Source code generated with Caramel.
-module(partial_functions).

-export([at_2/1]).
-export([at_3/1]).
-export([head/1]).
-export([one_el/1]).
-export([tail/1]).

head([X | _]) -> X.

tail([_ | Xs]) -> Xs.

one_el([X | []]) -> X.

at_2([_ | [X | _]]) -> X.

at_3([_ | [_ | [X | _]]]) -> X.


