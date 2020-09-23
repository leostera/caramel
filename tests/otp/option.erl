% Source code generated with Caramel.
-module(option).
-export_type([t/1]).


-type t(A) :: none
            | {some, A}
            .


