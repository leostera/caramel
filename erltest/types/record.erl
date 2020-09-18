% Source code generated with Caramel.
-module(record).
-export_type([inlined_record/0]).
-export_type([large_record/0]).
-export_type([record/1]).
-export_type([small_record/0]).


-type record(A) :: #{ author => list(A)
                    , year => integer()
                    , related => option:t(record(A))
                    }.

-type inlined_record() :: {simpler, boolean()}
                        | {many, boolean(), boolean()}
                        | {compound, #{ ir_a => float()
                                      , ir_b => boolean()
                                      }}
                        .

-type small_record() :: #{ a => string() }.

-type large_record() :: #{ lr_a => string()
                         , lr_b => string()
                         , lr_c => string()
                         , lr_d => string()
                         }.


