let head (x :: _) = x
let tail (_ :: xs) = xs

let one_el (x :: []) = x

let at_2 (_ :: x :: _) = x
let at_3 (_ :: _ :: x :: _) = x
