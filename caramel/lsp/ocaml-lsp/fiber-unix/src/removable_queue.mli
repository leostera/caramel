(** Like a queue, but allows O(1) removal at any position *)
type 'a t

type 'a node

val remove : _ node -> unit

val is_empty : _ t -> bool

val create : unit -> 'a t

val push : 'a t -> 'a -> 'a node

val pop : 'a t -> 'a option
