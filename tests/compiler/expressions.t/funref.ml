module Nested = struct
  let add x y = x + y
end

let f g = g ()
let rec g _ = f g

let add x y = x + y

let call_op_2 f x y = f x y

let do_add x y = call_op_2 add x y

let do_nested_add x y = call_op_2 Nested.add x y
