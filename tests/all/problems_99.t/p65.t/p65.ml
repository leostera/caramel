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

let rec height = function
  | Empty -> 0
  | Node (_, l, r) -> 1 + max (height l) (height r)

let rec find_missing_left tree_height depth = function
  | Empty -> tree_height - depth
  | Node (_, l, _) -> find_missing_left tree_height (depth + 1) l

let rec layout tree_height depth x_root = function
  | Empty -> Empty
  | Node (x, l, r) ->
      let spacing = 1 lsl (tree_height - depth - 1) in
      let l' = layout tree_height (depth + 1) (x_root - spacing) l
      and r' = layout tree_height (depth + 1) (x_root + spacing) r in
      Node ((x, x_root, depth), l', r')

let layout_binary_tree_2 t =
  let tree_height = height t in
  let translate_dst = (1 lsl find_missing_left tree_height 0 t) - 1 in
  layout tree_height 1 ((1 lsl (tree_height - 1)) - translate_dst) t

let main _ =
  let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node
      ( "n",
        Node
          ("k", Node ("c", leaf "a", Node ("e", leaf "d", leaf "g")), leaf "m"),
        Node ("u", Node ("p", Empty, leaf "q"), Empty) )
  in

  format "~p\n" [ layout_binary_tree_2 example_layout_tree ]
