
let iff cond t f =
  match cond with
  | true -> t
  | _ -> f

let is_empty l =
  match l with
  | [] -> true
  | _ :: _ -> false
