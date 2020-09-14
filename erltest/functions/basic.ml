(* type 'a pair = 'a * 'a *)

let pair x y = x, y

let left l _ = l
let right _ r = r

let fst (a, _) = a
let snd (_, b) = b

