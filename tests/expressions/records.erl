% Source code generated with Caramel.
-module(records).
-export_type([pair/1]).

-export([flatten_first/1]).
-export([fst/1]).
-export([map/3]).
-export([pair/2]).
-export([snd/1]).
-export([swap/1]).
-export([swap_from_expr/2]).

-type pair(A) :: #{ fst => A
                  , snd => A
                  }.

pair(X, Y) ->
  #{ fst => X
   , snd => Y
   }.

fst(#{ fst := Fst }) -> Fst.

snd(#{ snd := Snd }) -> Snd.

swap(P) ->
  #{ fst => maps:get(snd, P)
   , snd => maps:get(fst, P)
   }.

map(F, G, #{ fst := Fst, snd := Snd }) ->
  #{ fst => F(Fst)
   , snd => G(Snd)
   }.

swap_from_expr(P, F) ->
  #{ fst => maps:get(snd, F(P))
   , snd => maps:get(fst, F(P))
   }.

flatten_first(P) ->
  #{ fst => maps:get(fst, maps:get(fst, maps:get(fst, P)))
   , snd => maps:get(snd, maps:get(fst, maps:get(fst, P)))
   }.


