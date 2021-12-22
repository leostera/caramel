type t = { x: int; y: int; z: int }

let f () =
  let a = { x=0; y=0; z=0 } in
  { a with x=2; y=2 }
