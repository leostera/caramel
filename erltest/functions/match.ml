
let iff cond t f =
  if cond then t () else f ()

let is_empty l =
  match l with
  | [] -> true
  | _ :: _ -> false
