-module(rebind).

-export([f/1,g/1]).

f(X) -> case X of Y -> ok end.

g(Z) -> case Z of [Z] -> Z + 1 end.
