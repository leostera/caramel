(* FIXME: arity of run_local reference is off, it's 1 when it should be 0
 *)
let rec run_local () =
  let x = run_local in
  let y = `Atom in
  y

module Nested = struct
  let x () = `compiler "caramel"

  let w () = `version `kind_of_working
end

let rec run_nested () =
  Nested.x ();
  Nested.w ()

let run_nested_ambiguous () =
  let x () = 1 in
  Nested.x ()
