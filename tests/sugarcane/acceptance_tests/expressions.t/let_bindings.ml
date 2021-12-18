let let_one () =
  let a = 1 in
  a

let let_ignore () =
  let _ = 1 in
  2

let let_many () =
  let a = 1 in
  let b = 2 in
  let c = 3 in
  let d = 4 in
  a + b + c + d

let let_nested f g h =
  let a =
    g ();
    (* <-- a wild side-effect appears! D: *)
    let b =
      h ();
      let c = 1 in
      c + 1
    in
    b + 1
  in
  f a

let let_nested_another _ =
  let one = 1 in
  let three =
    let two = one + 1 in
    two + 1
  in
  three
