% Source code generated with Caramel.
-module(funref).

-export([add/2]).
-export([call_op_2/3]).
-export([do_add/2]).
-export([do_nested_add/2]).

add(X, Y) -> erlang:'+'(X, Y).

call_op_2(F, X, Y) -> F(X, Y).

do_add(X, Y) -> call_op_2(fun add/2, X, Y).

do_nested_add(X, Y) -> call_op_2(funref__nested:add, X, Y).


