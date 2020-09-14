% Source code generated with Caramel.
-module(records).

-export_type([pair/1]).

-export([fst/1]).
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


