open Import

type t

val create : unit -> t

type run_error =
  | Never
  | Abort_requested
  | Exn of Exn.t

val run_result : t -> 'a Fiber.t -> ('a, run_error) result

val run : t -> 'a Fiber.t -> 'a

type thread

type 'a task

val create_thread : t -> thread

val await : 'a task -> ('a, [ `Exn of Exn.t | `Canceled ]) result Fiber.t

val await_no_cancel : 'a task -> 'a Or_exn.t Fiber.t

val cancel_task : 'a task -> unit Fiber.t

val async : thread -> (unit -> 'a) -> ('a task, [ `Stopped ]) result

val async_exn : thread -> (unit -> 'a) -> 'a task

val stop : thread -> unit

type timer

val create_timer : t -> delay:float -> timer

val set_delay : timer -> delay:float -> unit

val schedule :
  timer -> (unit -> 'a Fiber.t) -> ('a, [ `Cancelled ]) result Fiber.t

val cancel_timer : timer -> unit Fiber.t

val cancel_timers : t -> unit Fiber.t

val scheduler : unit -> t

val abort : t -> unit
