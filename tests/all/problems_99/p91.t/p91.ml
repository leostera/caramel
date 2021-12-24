external ( && ) : bool -> bool -> bool = "erlang:and"

external ( * ) : int -> int -> int = "erlang:*"

external ( + ) : int -> int -> int = "erlang:+"

external ( - ) : int -> int -> int = "erlang:-"

external ( / ) : int -> int -> int = "erlang:div"

external ( < ) : int -> int -> bool = "erlang:<"

external ( <= ) : int -> int -> bool = "erlang:=<"

external ( >= ) : int -> int -> bool = "erlang:>="

external ( <> ) : int -> int -> bool = "erlang:=/="

external ( = ) : 'a -> 'a -> bool = "erlang:=="

external ( > ) : int -> int -> bool = "erlang:>"

external ( lsl ) : int -> int -> int = "erlang:bsl"

external ( mod ) : int -> int -> int = "erlang:rem"

external ( || ) : bool -> bool -> bool = "erlang:or"

external abs : int -> int = "erlang:abs"

external append : 'a list list -> 'a list = "lists:append"

external format : string -> 'a list -> unit = "io:format"

external length : 'a list -> int = "erlang:length"

external list_to_binary : 'a list -> string = "erlang:list_to_binary"

external max : int -> int -> int = "erlang:max"

external min : int -> int -> int = "erlang:min"

external neg : int -> int = "erlang:-"

external not : bool -> bool = "erlang:not"

external parse_int : string -> int = "erlang:list_to_integer"

external throw : 'a -> 'b = "erlang:throw"

let ( ^ ) a b = list_to_binary [ a; b ]

let ( @ ) a b = append [ a; b ]

let compare a b = if a <= b then 1 else if a = b then 0 else -1

let fst (x, _) = x

let snd (_, y) = y

module List = struct
  external map : ('a -> 'b) -> 'a list -> 'b list = "lists:map"

  external filter : ('a -> bool) -> 'a list -> 'a list = "lists:filter"

  external all : ('a -> bool) -> 'a list -> bool = "lists:all"

  external concat : 'a list list -> 'a list = "lists:concat"

  external hd : 'a list -> 'a = "erlang:hd"

  external tl : 'a list -> 'a list = "erlang:tl"

  external rev : 'a list -> 'a list = "lists:reverse"

  external member : 'a -> 'a list -> bool = "lists:member"

  external fold_left : ('el -> 'acc -> 'acc) -> 'acc -> 'el list -> 'acc
    = "lists:foldl"

  external fold_right : ('el -> 'acc -> 'acc) -> 'acc -> 'el list -> 'acc
    = "lists:foldr"

  let rec assoc x ls =
    match ls with
    | [] -> throw `not_found
    | (x', y) :: rest when x = x' -> y
    | _ :: rest -> assoc x rest
end

module Random = struct
  external seed : [ `default ] -> int -> unit = "rand:seed"

  external uniform : int -> int = "rand:uniform"
end

module Binary = struct
  external size : string -> int = "erlang:size"
end

let possible row col used_rows usedD1 usedD2 =
  not
    (List.member row used_rows
    || List.member (row + col) usedD1
    || List.member (row - col) usedD2)

let rec aux n row col used_rows usedD1 usedD2 =
  if col > n then [ List.rev used_rows ]
  else
    (if row < n then aux n (row + 1) col used_rows usedD1 usedD2 else [])
    @
    if possible row col used_rows usedD1 usedD2 then
      aux n 1 (col + 1) (row :: used_rows) ((row + col) :: usedD1)
        ((row - col) :: usedD2)
    else []

let queens_positions n = aux n 1 1 [] [] []

let main _ = format "~p\n" [ queens_positions 4 ]
