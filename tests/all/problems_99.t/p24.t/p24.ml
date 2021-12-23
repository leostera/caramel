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

external rev : 'a list -> 'a list = "lists:reverse"

module Random = struct
  external seed : [ `default ] -> int -> unit = "rand:seed"

  external uniform : int -> int = "rand:uniform"
end

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

let rec range_aux a b = if a > b then [] else a :: range_aux (a + 1) b

let range a b = if a > b then rev (range_aux b a) else range_aux a b

let lotto_select n m = rand_select (range 1 m) n

let main (n0 :: n1 :: _) =
  Random.seed `default 0;
  format "~p\n" [ lotto_select (parse_int n0) (parse_int n1) ]
