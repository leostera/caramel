external parse_int : string -> int = "erlang:list_to_integer"

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external format : string -> 'a list -> unit = "io:format"

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec aux n i = function
  | [] -> []
  | h :: t -> if i = n then aux n 1 t else h :: aux n (i + 1) t

let drop n list = aux n 1 list

let main (n :: xs) = format "~p\n" [ drop (parse_int n) xs ]
