type int_pair = { fst: int; snd: int }
let match_record () =
  match { fst=0; snd=1 } with
  | {fst; snd} -> true
  | {fst=x} -> true

let match_list () =
  match [0; 1] with
  | [] -> true
  | x :: [] -> true
  | x :: xs -> true

let match_tuples () =
  match 1, true, "hello" with
  | x, y, z -> true
  | x, _, _ -> true
  | x -> true

let match_atoms () =
  match `Hello with
  | x -> true
