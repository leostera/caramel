let empty () = []

let pair x = [x; x]

let cons x y = x :: y

let head (x :: _) = x

let tail (_ :: x) = x

let at_2 (_ :: x :: _) = x

let concat a b = a @ b
