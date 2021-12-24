external not : bool -> bool = "erlang:not"

external ( lsl ) : int -> int -> int = "erlang:bsl"

external list_to_binary : 'a list -> string = "erlang:list_to_binary"

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

type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

let rec layout depth x_left = function
  | Empty -> (Empty, x_left)
  | Node (v, l, r) ->
      let l', l_x_max = layout (depth + 1) x_left l in
      let r', r_x_max = layout (depth + 1) (l_x_max + 1) r in
      (Node ((v, l_x_max, depth), l', r'), r_x_max)

let layout_binary_tree_1 t = fst (layout 1 1 t)

let main _ =
  let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node
      ( "n",
        Node
          ( "k",
            Node ("c", leaf "a", Node ("h", Node ("g", leaf "e", Empty), Empty)),
            leaf "m" ),
        Node ("u", Node ("p", Empty, Node ("s", leaf "q", Empty)), Empty) )
  in
  format "~p\n" [ layout_binary_tree_1 example_layout_tree ]
