% Source code generated with Caramel.
-module(labeled_arguments).

-export([concat/2]).
-export([run/0]).

concat(A, B) -> << A/binary, B/binary >>.

run() ->
  S1 = concat(<<"ocaml">>, <<"erlang">>),
  S2 = concat(<<"erlang">>, <<"ocaml">>),
  erlang:'=:='(S1, S2).


