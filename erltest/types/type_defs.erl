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


-type alias() :: int().

-type opaque() :: fun((alias()) -> bool()).

-type hidden() :: ref().

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

-type phantom(A) :: phantom
                  | {phantom_with_value, int()}
                  .

-type small_record() :: #{ a => string() }.

-type large_record() :: #{ lr_a => string()
                         , lr_b => string()
                         , lr_c => string()
                         , lr_d => string()
                         }.

-type inlined_record() :: {simpler, bool()}
                        | {many, bool(), bool()}
                        | {compound, #{ ir_a => float()
                                      , ir_b => bool()
                                      }}
                        .

-type compound() :: #{ c_a => inlined_record()
                     , c_b => phantom(bool())
                     , c_c => triplet(int(), bool())
                     , c_d => large_record()
                     , c_fn => fun((unit()) -> {small_record(), int()})
                     }.


