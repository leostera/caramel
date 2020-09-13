% Source code generated with Caramel.
-module(type_defs).

-export_type([alias/0]).
-export_type([compound/0]).
-export_type([hidden/0]).
-export_type([inlined_record/0]).
-export_type([list/1]).
-export_type([opaque/0]).
-export_type([option/1]).
-export_type([phantom/1]).
-export_type([poly/0]).
-export_type([record/0]).
-export_type([result/2]).
-export_type([tree/1]).
-export_type([triplet/2]).

-type alias() :: int().

-type opaque() :: list(string()).

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

-type phantom(_A) :: phantom
                  | {phantom_with_value, int()}
                  .

-type record() :: #{ a => string()
                   , b => int()
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
                     , c_d => record()
                     }.


