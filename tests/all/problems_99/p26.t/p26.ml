external ( + ) : 'a -> 'a -> 'a = "erlang:+"

external ( - ) : 'a -> 'a -> 'a = "erlang:-"

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( > ) : int -> int -> bool = "erlang:>"

external ( <= ) : int -> int -> bool = "erlang:=<"

external ( mod ) : int -> int -> int = "erlang:rem"

external append : 'a list list -> 'a list = "lists:append"

external format : string -> 'a list -> unit = "io:format"

external length : 'a list -> int = "erlang:length"

external min : int -> int -> int = "erlang:min"

external parse_int : string -> int = "erlang:list_to_integer"

external throw : 'a -> 'b = "erlang:throw"

external rev : 'a list -> 'a list = "lists:reverse"

let ( @ ) a b = append [ a; b ]

module List = struct
  external map : ('a -> 'b) -> 'a list -> 'b list = "lists:map"
end

module Random = struct
  external seed : [ `default ] -> int -> unit = "rand:seed"

  external uniform : int -> int = "rand:uniform"
end

let rec extract k list =
  if k <= 0 then [ [] ]
  else
    match list with
    | [] -> []
    | h :: tl ->
        let with_h = List.map (fun l -> h :: l) (extract (k - 1) tl) in
        let without_h = extract k tl in
        with_h @ without_h

let main (n :: xs) = format "~p\n" [ extract (parse_int n) xs ]
