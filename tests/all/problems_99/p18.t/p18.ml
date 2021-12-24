external parse_int : string -> int = "erlang:list_to_integer"

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external ( - ) : 'a -> 'a -> 'a = "erlang:-"

external format : string -> 'a list -> unit = "io:format"

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec take n = function
  | [] -> []
  | h :: t -> if n = 0 then [] else h :: take (n - 1) t

let rec drop n = function
  | [] -> []
  | h :: t as l -> if n = 0 then l else drop (n - 1) t


let slice list i k = take (k - i + 1) (drop i list)

let main (i0 :: i1 :: xs) = format "~p\n" [ slice xs (parse_int i0) (parse_int i1) ]
