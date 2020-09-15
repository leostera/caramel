% Source code generated with Caramel.
-module(binding_on_match).

-export([f/2]).

f(X) -> fun
   ([]) -> [];
   ([A | B]) -> [X(A) | f(X, B)]
 end.


