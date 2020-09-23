% Source code generated with Caramel.
-module(qualified_names).
-export_type([compound/0]).


-type compound() :: #{ c_a => record:inlined_record()
                     , c_b => type_args:phantom(boolean())
                     , c_c => type_args:triplet(integer(), boolean())
                     , c_d => record:large_record()
                     , c_fn => {record:small_record(), fn:defer(integer())}
                     }.


