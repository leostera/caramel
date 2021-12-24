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

external rev : 'a list -> 'a list = "lists:reverse"

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

  external fold_left : ('el -> 'acc -> 'acc) -> 'acc -> 'el list -> 'acc
    = "lists:foldl"

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

type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

let rec string_of_tree = function
  | Empty -> ""
  | Node (data, l, r) -> (
      match (l, r) with
      | Empty, Empty -> data
      | _, _ -> data ^ "(" ^ string_of_tree l ^ "," ^ string_of_tree r ^ ")")

let main _ =
  let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node
      ( "a",
        Node ("b", leaf "d", leaf "e"),
        Node ("c", Empty, Node ("f", leaf "g", Empty)) )
  in

  format "~p\n" [ string_of_tree example_layout_tree ]
