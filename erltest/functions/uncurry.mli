type ignore = unit -> unit

type 'a defer = unit -> 'a

val ignore : 'a -> ignore

val add : int -> int -> int

val add_slow : int -> int -> int defer

val add_really_slow : int -> (int -> int defer) defer
