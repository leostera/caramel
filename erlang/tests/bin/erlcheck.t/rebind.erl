-module(rebind).

f(X) -> case X of Y -> ok end.

g(Z) -> case Z of [Z] -> Z + 1 end.
