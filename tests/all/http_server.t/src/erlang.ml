type pid

type term

type atom

external atom : string -> atom = "erlang:binary_to_atom"

external as_term : 'a -> term = "%identity"

type ('ok, 'err) result = Ok of 'ok | Error of 'err

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

external length : 'a list -> int = "erlang:length"

external list_to_binary : 'a list -> string = "erlang:list_to_binary"

external max : int -> int -> int = "erlang:max"

external min : int -> int -> int = "erlang:min"

external neg : int -> int = "erlang:-"

external not : bool -> bool = "erlang:not"

external binary_to_integer : string -> int = "erlang:binary_to_integer"

external integer_to_binary : int -> string = "erlang:integer_to_binary"

external throw : 'a -> 'b = "erlang:throw"

module List = struct
  external append : 'a list list -> 'a list = "lists:append"
end

module Io = struct
  external format : string -> 'a list -> unit = "io:format"
end

module Io_lib = struct
  external format : string -> 'a list -> string = "io_lib:format"
end

let ( ^ ) a b = list_to_binary [ a; b ]

let ( @ ) a b = List.append [ a; b ]

let ( |> ) x f = f x

let compare a b = if a <= b then 1 else if a = b then 0 else -1

let fst (x, _) = x

let snd (_, y) = y

module Supervisor = struct
  type sup_strategy = One_for_all

  type sup_flags = { strategy : sup_strategy; intensity : int; period : int }

  type child_spec = { id : atom; start : atom * atom * term }

  type register = Local of atom

  external start_link : register -> 'b -> 'c list -> (pid, 'err) result
    = "supervisor:start_link"
end
