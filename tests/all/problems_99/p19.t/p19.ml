external parse_int : string -> int = "erlang:list_to_integer"

external append : 'a list list -> 'a list = "lists:append"

external ( mod ) : int -> int -> int = "erlang:rem"

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external ( - ) : 'a -> 'a -> 'a = "erlang:-"

external length : 'a list -> int = "erlang:length"

external format : string -> 'a list -> unit = "io:format"

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec aux i acc = function
  | [] -> (rev acc [], [])
  | h :: t as l -> if i = 0 then (rev acc [], l) else aux (i - 1) (h :: acc) t

let split list n = aux n [] list

let rotate list n =
  let len = length list in
  (* Compute a rotation value between 0 and len - 1 *)
  let n = if len = 0 then 0 else ((n mod len) + len) mod len in
  if n = 0 then list
  else
    let a, b = split list n in
    append [b; a]

let main (n :: xs) = format "~p\n" [ rotate xs (parse_int n) ]
