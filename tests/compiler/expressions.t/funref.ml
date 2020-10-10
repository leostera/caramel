module Nested = struct
  let add x y = x + y
end

let f g = g ()
let rec g _ = f g

let add x y = x + y

let call_op_2 f x y = f x y

let do_add x y = call_op_2 add x y

(* FIXME: currently this has lost track of the fact that it is indeed a funref
   and its instead using a Qualified_name

   It should produce:
     do_nested(X, Y) -> call_op_2(fun funref__nested:add/2, X, Y).
 *)
let do_nested_add x y = call_op_2 Nested.add x y
