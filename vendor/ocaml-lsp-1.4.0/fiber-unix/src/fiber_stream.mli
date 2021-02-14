module In : sig
  type 'a t

  val create : (unit -> 'a option Fiber.t) -> 'a t

  val empty : unit -> 'a t

  val of_list : 'a list -> 'a t

  val read : 'a t -> 'a option Fiber.t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val filter : 'a t -> f:('a -> bool) -> 'a t

  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

  val sequential_iter : 'a t -> f:('a -> unit Fiber.t) -> unit Fiber.t
end

module Out : sig
  type 'a t

  val create : ('a option -> unit Fiber.t) -> 'a t

  val write : 'a t -> 'a option -> unit Fiber.t

  val of_ref : 'a list ref -> 'a t

  val null : unit -> 'a t
end

(** [connect i o] reads from [i] and writes to [o]. Closes [o] when [i] is
    exhausted. Returned fiber terminates when [i] is exhausted *)
val connect : 'a In.t -> 'a Out.t -> unit Fiber.t

(** [supply i o] like [connect i o] but does not close [o] once [i] is
    exhausted. Returned fiber terminates when [i] is exhausted*)
val supply : 'a In.t -> 'a Out.t -> unit Fiber.t

val pipe : unit -> 'a In.t * 'a Out.t
