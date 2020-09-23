% Source code generated with Caramel.
-module(inductive).
-export_type([a_list/1]).
-export_type([tree/1]).


-type a_list(A) :: {cons, {A, a_list(A)}}
                 | nil
                 .

-type tree(Value) :: {leaf, Value, Value}
                   | {node, Value, tree(Value)}
                   .


