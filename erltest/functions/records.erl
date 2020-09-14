% Source code generated with Caramel.
-module(records).

-export_type([pair/1]).


-type pair(A) :: #{ fst => A
                  , snd => A
                  }.


