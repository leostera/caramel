(** Reprsents a bi-directional jsonrpc packet stream read in dedicated threads.

    TODO Nothing here is specific to jsonrpc *)
open! Import

open Jsonrpc

type t

val close : t -> unit Fiber.t

val send : t -> packet -> unit Fiber.t

val recv : t -> packet option Fiber.t

val make : Scheduler.t -> Io.t -> t
