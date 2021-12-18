open Erlang

let f x =
    match x with
    | y when (is_number y) -> 3
    | y when Erlang.is_binary y -> 3
    | y when y > 3 -> 3
    | y when y <= 3 -> 3
    | _ -> 4

