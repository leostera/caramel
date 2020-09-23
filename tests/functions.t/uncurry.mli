(* FIXME: this should compile to: fun() -> ok
 * since ok is the "unit" or Erlang for return calls
 *)
type ignore = unit -> unit

(* FIXME: this should compile to: fun() -> A
 * since "unit" should go away if its the only argument
 *)
type 'a defer = unit -> 'a

val ignore : 'a -> ignore

val add : int -> int -> int

val add_slow : int -> int -> int defer

val add_really_slow : int -> (int -> int defer) defer
