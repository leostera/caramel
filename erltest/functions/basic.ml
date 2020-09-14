(* type 'a pair = 'a * 'a *)

let pair x y = x, y

let fst (a, _) = a
let snd (_, b) = b
