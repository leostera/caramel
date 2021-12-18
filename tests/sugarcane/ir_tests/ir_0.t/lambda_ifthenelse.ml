let f () = if true then (1, 1) else (2, 2)

let g () = if false then ()

let as_match x = match x with false -> true | true -> false

let catchall0 x = match x with 1 -> true | _ -> false

let catchall1 x = match x with 1 -> true | 2 -> false | _ -> false

let catchall2 x = match x with 3 | 4 -> true | _ -> false
