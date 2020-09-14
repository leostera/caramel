
let add x = x

let double f x = f (f x)

let add_twice x = double add x

module Nested = struct
  let f x () =
    match x with
    | true -> ()
    | false -> ()
end

let call_nested x = Nested.f x ()

let call_other x = Call_other.f x

let call_other_nested x = Call_other.Nested.f x
