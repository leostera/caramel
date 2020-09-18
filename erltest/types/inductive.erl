% Source code generated with Caramel.
-module(inductive).
-export_type([list/1]).
-export_type([tree/1]).


-type list(A) :: {cons, {A, list(A)}}
               | nil
               .

-type tree(Value) :: {leaf, Value, Value}
                   | {node, Value, tree(Value)}
                   .


