open Erlang

let g _ = false

let f x =
    match x with
    | y when (g y) -> 3
    | _ -> 4

