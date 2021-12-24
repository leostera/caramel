external parse_int : string -> int = "erlang:list_to_integer"

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( - ) : 'a -> 'a -> 'a = "erlang:-"

external display : 'a -> unit = "erlang:display"

let rec at k = function
  | [] -> display "no match"
  | h :: t -> if k = 1 then display h else at (k - 1) t

let rec main (k :: ls) = at (parse_int k) ls
