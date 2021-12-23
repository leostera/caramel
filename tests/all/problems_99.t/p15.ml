external parse_int : string -> int = "erlang:list_to_integer"

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( - ) : 'a -> 'a -> 'a = "erlang:-"

external format : string -> 'a list -> unit = "io:format"

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec prepend n acc x = if n = 0 then acc else prepend (n - 1) (x :: acc) x

let rec aux n acc = function [] -> acc | h :: t -> aux n (prepend n acc h) t

let replicate n list = aux n [] (rev list [])

let main (n :: xs) = format "~p\n" [ replicate (parse_int n) xs ]
