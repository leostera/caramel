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

let rec preorder = function
  | Empty -> []
  | Node (v, l, r) -> v :: (preorder l @ preorder r)

let rec inorder = function
  | Empty -> []
  | Node (v, l, r) -> inorder l @ (v :: inorder r)

let rec split_pre_in p i x accp acci =
  match (p, i) with
  | [], [] -> ((List.rev accp, List.rev acci), ([], []))
  | h1 :: t1, h2 :: t2 ->
      if x = h2 then
        ( (List.tl (List.rev (h1 :: accp)), t1),
          (List.rev (List.tl (h2 :: acci)), t2) )
      else split_pre_in t1 t2 x (h1 :: accp) (h2 :: acci)
  | _ -> throw `could_not_split

let rec pre_in_tree p i =
  match (p, i) with
  | [], [] -> Empty
  | h1 :: t1, h2 :: t2 ->
      let (lp, rp), (li, ri) = split_pre_in p i h1 [] [] in
      Node (h1, pre_in_tree lp li, pre_in_tree rp ri)
  | _ -> throw `pre_in_tree

let main _ =
  format "~p\n"
    [
      [
        preorder (Node (1, Node (2, Empty, Empty), Empty));
        preorder (Node (1, Empty, Node (2, Empty, Empty)));
      ];
    ]
