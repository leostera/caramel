type ignore = unit -> unit

(* NOTE: need to figure out how to handle this case in an "obvious to Erlang"
 * way *)
let ignore _x () = ()


type 'a defer = unit -> 'a

let add x y = x + x

(* TODO: these 2 should actually compile to a function of arity 2 and 1
 * that returns lambdas.
 *)
let add_slow x y () = x + y

let add_really_slow x () y () = x + y
