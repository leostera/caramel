% Source code generated with Caramel.
-module(record).

-export_type([record/1]).


-type record(A) :: #{ author => list(A)
                    , year => integer()
                    }.


