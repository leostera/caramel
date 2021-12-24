external parse_int : string -> int = "erlang:list_to_integer"

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external ( - ) : 'a -> 'a -> 'a = "erlang:-"

external format : string -> 'a list -> unit = "io:format"

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec aux i acc = function
  | [] -> (rev acc [], [])
  | h :: t as l -> if i = 0 then (rev acc [], l) else aux (i - 1) (h :: acc) t

let split n list = aux n [] list

let main (n :: xs) = format "~p\n" [ split (parse_int n) xs ]

