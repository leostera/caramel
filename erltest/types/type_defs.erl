% Source code generated with Caramel.
-module(type_defs).

-export_type([alias/0]).
-export_type([compound/0]).
-export_type([hidden/0]).
-export_type([inlined_record/0]).
-export_type([large_record/0]).
-export_type([list/1]).
-export_type([opaque/0]).
-export_type([option/1]).
-export_type([phantom/1]).
-export_type([result/2]).
-export_type([small_record/0]).
-export_type([tree/1]).
-export_type([triplet/2]).


-type alias() :: integer().

-type opaque() :: fun((alias()) -> boolean()).

-type hidden() :: reference().

-type list(A) :: {cons, {A, list(A)}}
               | nil
               .

-type tree(Value) :: {leaf, Value}
                   | {node, Value, tree(Value), Value}
                   .

-type option(A) :: {some, A}
                 | none
                 .

-type result(A, B) :: {ok, A}
                    | {error, B}
                    .

-type triplet(A, B) :: {A, A, B}.

-type phantom(_A) :: phantom
                   | {phantom_with_value, integer()}
                   .

-type small_record() :: #{ a => string() }.

-type large_record() :: #{ lr_a => string()
                         , lr_b => string()
                         , lr_c => string()
                         , lr_d => string()
                         }.

-type inlined_record() :: {simpler, boolean()}
                        | {many, boolean(), boolean()}
                        | {compound, #{ ir_a => float()
                                      , ir_b => boolean()
                                      }}
                        .

-type compound() :: #{ c_a => inlined_record()
                     , c_b => phantom(boolean())
                     , c_c => triplet(integer(), boolean())
                     , c_d => large_record()
                     , c_fn => fun((unit()) -> {small_record(), integer()})
                     }.


