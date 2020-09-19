type ignore = unit -> unit

(* FIXME: this should compile to
 *
 *  ignore(_X) -> fun() -> ok end.
 *)
let ignore _x () = ()

type 'a defer = unit -> 'a

let add x y = x + x

(* FIXME: should compile to:
 *
 *  add_slow(X, Y) -> fun() -> ... end.
 *)
let add_slow x y () = x + y

(* NOTE: very debatable. Need to discuss more.
 *
 * FIXME: this should compile to
 *
 *  add_really_slow(X) -> fun(Y) -> fun() -> erlang:'+'(X, Y) end end.
 *
 *)
let add_really_slow x () y () = x + y
