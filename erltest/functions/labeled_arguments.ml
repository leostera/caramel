
let concat ~a ~b = a ^ b

(* FIXME: while this compiles, it is ignoring the label
 * information and these two calls are equivalent
 *)
let run () =
  let s1 = concat ~b:"erlang" ~a:"ocaml" in
  let s2 = concat ~a:"erlang" ~b:"ocaml" in
  s1 = s2

