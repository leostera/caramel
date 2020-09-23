let add x = x

let double f x = f (f x)

let add_twice x = double add x

module Nested = struct
  (* FIXME: arity of f here is off by one when the last argument is () *)
  let f x = match x with true -> () | false -> ()
end

let call_nested x = Nested.f x

let call_other x = Qualified_calls_helper.f x

let call_other_nested x = Qualified_calls_helper.Nested.f x
