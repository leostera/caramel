external ( && ) : bool -> bool -> bool = "erlang:and"

external ( * ) : int -> int -> int = "erlang:*"

external ( + ) : int -> int -> int = "erlang:+"

external ( - ) : int -> int -> int = "erlang:-"

external ( / ) : int -> int -> int = "erlang:div"

external ( < ) : int -> int -> bool = "erlang:<"

external ( <= ) : int -> int -> bool = "erlang:=<"

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

type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

let rec translate_x d = function
  | Empty -> Empty
  | Node ((v, x, y), l, r) ->
      Node ((v, x + d, y), translate_x d l, translate_x d r)

let rec dist lr rl =
  match (lr, rl) with
  | lrx :: ltl, rlx :: rtl -> max (lrx - rlx) (dist ltl rtl)
  | [], _ | _, [] -> 0

let rec merge_profiles p1 p2 =
  match (p1, p2) with
  | x1 :: tl1, _ :: tl2 -> x1 :: merge_profiles tl1 tl2
  | [], _ -> p2
  | _, [] -> p1

let rec layout depth = function
  | Empty -> ([], Empty, [])
  | Node (v, l, r) ->
      let ll, l', lr = layout (depth + 1) l in
      let rl, r', rr = layout (depth + 1) r in
      let d = 1 + (dist lr rl / 2) in
      let ll = List.map (fun x -> x - d) ll in
      let lr = List.map (fun x -> x - d) lr in
      let rl = List.map (fun x -> x + d) rl in
      let rr = List.map (fun x -> x + d) rr in
      ( 0 :: merge_profiles ll rl,
        Node ((v, 0, depth), translate_x (neg d) l', translate_x d r'),
        0 :: merge_profiles rr lr )

let layout_binary_tree_3 t =
  let l, t', _ = layout 1 t in
  let x_min = List.fold_left min 0 l in
  translate_x (1 - x_min) t'

let main _ =
  let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node
      ( "n",
        Node
          ("k", Node ("c", leaf "a", Node ("e", leaf "d", leaf "g")), leaf "m"),
        Node ("u", Node ("p", Empty, leaf "q"), Empty) )
  in

  format "~p\n" [ layout_binary_tree_3 example_layout_tree ]
