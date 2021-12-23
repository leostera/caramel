external not : bool -> bool = "erlang:not"

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

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec eval2 a val_a b val_b = function
  | Var x ->
      if x = a then val_a
      else if x = b then val_b
      else throw "The expression contains an invalid variable"
  | Not e -> not (eval2 a val_a b val_b e)
  | And (e1, e2) -> eval2 a val_a b val_b e1 && eval2 a val_a b val_b e2
  | Or (e1, e2) -> eval2 a val_a b val_b e1 || eval2 a val_a b val_b e2

let table2 a b expr =
  [
    (true, true, eval2 a true b true expr);
    (true, false, eval2 a true b false expr);
    (false, true, eval2 a false b true expr);
    (false, false, eval2 a false b false expr);
  ]

let main _ =
  format "~p\n" [ table2 "a" "b" (And (Var "a", Or (Var "a", Var "b"))) ]
