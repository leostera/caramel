let iff_using_headers true f _ = f
(* TODO: make redefining a function a compiler error:
let iff false _ f = f
*)

let iff_using_function =
  function
  | false, _, f -> f
  | true, f, _ -> f

let iff_using_if cond t f =
  if cond then t () else f ()

let iff_using_match t f g =
  match t with
  | true -> f
  | false -> g
