% Source code generated with Caramel.
-module(variants).
-export_type([option/1]).
-export_type([result/2]).


-type option(A) :: {some, A}
                 | none
                 .

-type result(A, B) :: {ok, A}
                    | {error, B}
                    .


