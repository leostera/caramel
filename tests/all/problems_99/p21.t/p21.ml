external parse_int : string -> int = "erlang:list_to_integer"

external append : 'a list list -> 'a list = "lists:append"

external ( mod ) : int -> int -> int = "erlang:rem"

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external ( - ) : 'a -> 'a -> 'a = "erlang:-"

external length : 'a list -> int = "erlang:length"

external format : string -> 'a list -> unit = "io:format"

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec insert_at x n = function
  | [] -> [ x ]
  | h :: t as l -> if n = 0 then x :: l else h :: insert_at x (n - 1) t

let main (el :: n :: xs) = format "~p\n" [ insert_at el (parse_int n) xs ]
