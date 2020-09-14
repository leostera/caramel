% Source code generated with Caramel.
-module(list).

-export_type([list/1]).


-type list(A) :: {cons, {A, list(A)}}
               | nil
               .


