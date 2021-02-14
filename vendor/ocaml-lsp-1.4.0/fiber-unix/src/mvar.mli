type 'a t

val create : unit -> 'a t

val get : 'a t -> 'a

val set : 'a t -> 'a -> unit
