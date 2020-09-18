% Source code generated with Caramel.
-module(records).
-export_type([pair/1]).

-export([fst/1]).
-export([map/3]).
-export([pair/2]).
-export([snd/1]).

-type pair(A) :: #{ fst => A
                  , snd => A
                  }.

pair(X, Y) ->
  #{ fst => X
   , snd => Y
   }.

fst(#{ fst := Fst }) -> Fst.

snd(#{ snd := Snd }) -> Snd.

map(F, G, #{ fst := Fst, snd := Snd }) ->
  #{ fst => F(Fst)
   , snd => G(Snd)
   }.


