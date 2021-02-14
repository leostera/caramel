type t

val create : unit -> t

val task : t -> f:(unit -> unit Fiber.t) -> (unit, [ `Stopped ]) result Fiber.t

val task_exn : t -> f:(unit -> unit Fiber.t) -> unit Fiber.t

val stop : t -> unit Fiber.t

val run : t -> unit Fiber.t
