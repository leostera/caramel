% Source code generated with Caramel.
-module(type_args).
-export_type([phantom/1]).
-export_type([triplet/2]).


-type triplet(A, B) :: {A, A, B}.

-type phantom(_A) :: phantom
                   | {phantom_with_value, integer()}
                   .


