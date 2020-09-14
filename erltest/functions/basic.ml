let pair x y = x, y

let left l _ = l
let right _ r = r

let fst (a, _) = a
let snd (_, b) = b

let combine (a, b) (c, d) = ((a, c), (b, d))

let head (x :: _) = x
let tail (_ :: xs) = xs

let one_el (x :: []) = x

let at_2 (_ :: x :: _) = x
let at_3 (_ :: _ :: x :: _) = x

let iff true f _ = f
let iff false _ f = f

let iff2 =
  function
  | false, _, f -> f
  | true, f, _ -> f

let ignore _x = ()

let empty_list _x = []
