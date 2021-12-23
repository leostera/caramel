external not : bool -> bool = "erlang:not"

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

module List = struct
  external map : ('a -> 'b) -> 'a list -> 'b list = "lists:map"

  external filter : ('a -> bool) -> 'a list -> 'a list = "lists:filter"

  external all : ('a -> bool) -> 'a list -> bool = "lists:all"

  external concat : 'a list list -> 'a list = "lists:concat"

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

let add_trees_with left right all =
  let add_right_tree all l =
    List.fold_left (fun r a -> Node ("x", l, r) :: a) all right
  in
  List.fold_left (fun el acc -> add_right_tree acc el) all left

let rec cbal_tree n =
  if n = 0 then [ Empty ]
  else if n mod 2 = 1 then
    let t = cbal_tree (n / 2) in
    add_trees_with t t []
  else
    (* n even: n-1 nodes for the left & right subtrees altogether. *)
    let t1 = cbal_tree ((n / 2) - 1) in
    let t2 = cbal_tree (n / 2) in
    add_trees_with t1 t2 (add_trees_with t2 t1 [])

let rec is_mirror t1 t2 =
  match (t1, t2) with
  | Empty, Empty -> true
  | Node (_, l1, r1), Node (_, l2, r2) -> is_mirror l1 r2 && is_mirror r1 l2
  | _ -> false

let is_symmetric = function Empty -> true | Node (_, l, r) -> is_mirror l r

let sym_cbal_trees n = List.filter (fun x -> is_symmetric x) (cbal_tree n)

let rec hbal_tree n =
  if n = 0 then [ Empty ]
  else if n = 1 then [ Node ("x", Empty, Empty) ]
  else
    let t1 = hbal_tree (n - 1) and t2 = hbal_tree (n - 2) in
    add_trees_with t1 t1 (add_trees_with t1 t2 (add_trees_with t2 t1 []))

let main _ = format "~p\n" [ hbal_tree 3 ]
