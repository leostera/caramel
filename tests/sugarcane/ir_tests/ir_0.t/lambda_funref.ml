let g x = (x, 1)

let rec map f x = match x with [] -> [] | y :: xs -> f y :: map f xs

let f args =
  let h x = (x, 2) in
  (map h args, map g args, map Lambda_utils.dupe args)

let f2 args =
  let g x = (x, 2) in
  (map g args, map Lambda_utils.dupe args)
