external ( + ) : int -> int -> int = "erlang:+"

external ( / ) : int -> int -> int = "erlang:div"

external ( * ) : int -> int -> int = "erlang:*"

external ( - ) : int -> int -> int = "erlang:-"

external ( <= ) : int -> int -> bool = "erlang:=<"

external ( < ) : int -> int -> bool = "erlang:<"

external ( <> ) : int -> int -> bool = "erlang:=/="

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( > ) : int -> int -> bool = "erlang:>"

external ( mod ) : int -> int -> int = "erlang:rem"

external ( || ) : bool -> bool -> bool = "erlang:or"

external ( && ) : bool -> bool -> bool = "erlang:and"

external abs : int -> int = "erlang:abs"

external append : 'a list list -> 'a list = "lists:append"

external format : string -> 'a list -> unit = "io:format"

external length : 'a list -> int = "erlang:length"

external min : int -> int -> int = "erlang:min"

external max : int -> int -> int = "erlang:max"

external parse_int : string -> int = "erlang:list_to_integer"

external rev : 'a list -> 'a list = "lists:reverse"

external throw : 'a -> 'b = "erlang:throw"

let ( @ ) a b = append [ a; b ]

let compare a b = if a <= b then 1 else if a = b then 0 else -1

module List = struct
  external map : ('a -> 'b) -> 'a list -> 'b list = "lists:map"

  external filter : ('a -> bool) -> 'a list -> 'a list = "lists:filter"

  external all : ('a -> bool) -> 'a list -> bool = "lists:all"

  external concat : 'a list list -> 'a list = "lists:concat"
end

module Random = struct
  external seed : [ `default ] -> int -> unit = "rand:seed"

  external uniform : int -> int = "rand:uniform"
end

let rec is_not_divisor n d =
  if d * d > n then true else n mod d <> 0 && is_not_divisor n (d + 1)

let is_prime n =
  let n = abs n in
  is_not_divisor n 2

let rec goldbach_aux n d =
  if is_prime d && is_prime (n - d) then (d, n - d) else goldbach_aux n (d + 1)

let goldbach n = goldbach_aux n 2

let rec goldbach_list a b =
  if a > b then []
  else if a mod 2 = 1 then goldbach_list (a + 1) b
  else (a, goldbach a) :: goldbach_list (a + 2) b

let goldbach_limit a b lim =
  List.filter (fun (_, (a, b)) -> a > lim && b > lim) (goldbach_list a b)

let main _ =
  format "~p\n"
    [
      [
        ("goldbach_list 9 20", goldbach_list 9 20);
        ("goldbach_limit 1 2000 50", goldbach_limit 1 2000 50);
      ];
    ]
