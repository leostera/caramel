external parse_int : string -> int = "erlang:list_to_integer"

external append : 'a list list -> 'a list = "lists:append"

external ( mod ) : int -> int -> int = "erlang:rem"

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external ( > ) : int -> int -> bool = "erlang:>"

external length : 'a list -> int = "erlang:length"

external format : string -> 'a list -> unit = "io:format"

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec aux a b = if a > b then [] else a :: aux (a + 1) b

let range a b = if a > b then rev (aux b a) [] else aux a b

let main (a :: b :: _) = format "~p\n" [ range (parse_int a) (parse_int b) ]
