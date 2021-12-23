external parse_int : string -> int = "erlang:list_to_integer"

external append : 'a list list -> 'a list = "lists:append"

external ( mod ) : int -> int -> int = "erlang:rem"

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external ( - ) : 'a -> 'a -> 'a = "erlang:-"

external length : 'a list -> int = "erlang:length"

external format : string -> 'a list -> unit = "io:format"

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec remove_at n = function
  | [] -> []
  | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t

let main (n :: xs) = format "~p\n" [ remove_at (parse_int n) xs ]
