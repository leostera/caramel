external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external ( - ) : 'a -> 'a -> 'a = "erlang:-"

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( > ) : int -> int -> bool = "erlang:>"

external ( mod ) : int -> int -> int = "erlang:rem"

external append : 'a list list -> 'a list = "lists:append"

external format : string -> 'a list -> unit = "io:format"

external length : 'a list -> int = "erlang:length"

external min : int -> int -> int = "erlang:min"

external parse_int : string -> int = "erlang:list_to_integer"

external throw : 'a -> 'b = "erlang:throw"

module Random = struct
  external seed : [ `default ] -> int -> unit = "rand:seed"

  external uniform : int -> int = "rand:uniform"
end

let rec rev list acc = match list with [] -> acc | h :: t -> rev t (h :: acc)

let rec extract acc n = function
  | [] -> None
  | h :: t ->
      if n = 0 then Some (h, append [ acc; t ])
      else extract (h :: acc) (n - 1) t

let extract_rand list len = extract [] (Random.uniform len) list

let rec aux n acc list len =
  if n = 0 then acc
  else
    let (Some (picked, rest)) = extract_rand list len in
    aux (n - 1) (picked :: acc) rest (len - 1)

let rand_select list n =
  let len = length list in
  aux (min n len) [] list len

let main (n :: xs) =
  Random.seed `default 0;
  format "~p\n" [ rand_select xs (parse_int n) ]
