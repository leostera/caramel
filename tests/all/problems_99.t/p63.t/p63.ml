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

let rec split_n lst acc n =
  match (n, lst) with
  | 0, _ -> (rev acc, lst)
  | _, [] -> (rev acc, [])
  | _, h :: t -> split_n t (h :: acc) (n - 1)

let rec myflatten p c =
  match (p, c) with
  | p, [] -> List.map (fun x -> Node (x, Empty, Empty)) p
  | x :: t, [ y ] -> Node (x, y, Empty) :: myflatten t []
  | ph :: pt, x :: y :: t -> Node (ph, x, y) :: myflatten pt t
  | _ -> throw `oops

let rec aux l = function
  | [] -> []
  | lst ->
      let p, c = split_n lst [] (1 lsl l) in
      myflatten p (aux (l + 1) c)

let complete_binary_tree = function [] -> Empty | lst -> List.hd (aux 0 lst)

let main _ =
  let _example =
    Node
      ( "a",
        Node ("b", Node ("d", Empty, Empty), Node ("e", Empty, Empty)),
        Node ("c", Empty, Node ("f", Node ("g", Empty, Empty), Empty)) )
  in
  format "~p\n" [ complete_binary_tree [ 1; 2; 3; 4; 5; 6 ] ]
