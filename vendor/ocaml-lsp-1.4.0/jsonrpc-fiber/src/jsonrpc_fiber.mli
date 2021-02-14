open Import
open Jsonrpc

module Notify : sig
  type t =
    | Stop
    | Continue
end

module Reply : sig
  type t

  val now : Response.t -> t

  val later : ((Response.t -> unit Fiber.t) -> unit Fiber.t) -> t
end

(** IO free implementation of the jsonrpc protocol. We stay completely agnostic
    of transport by only dealing with abstract jsonrpc packets *)
module Make (Chan : sig
  type t

  val send : t -> packet -> unit Fiber.t

  val recv : t -> packet option Fiber.t

  val close : t -> unit Fiber.t
end) : sig
  type 'state t

  module Context : sig
    type ('state, 'req) t

    type 'a session

    val message : (_, 'req) t -> 'req Message.t

    val state : ('a, _) t -> 'a

    val session : ('a, _) t -> 'a session
  end
  with type 'a session := 'a t

  val create :
       ?on_request:(('state, Id.t) Context.t -> (Reply.t * 'state) Fiber.t)
    -> ?on_notification:
         (('state, unit) Context.t -> (Notify.t * 'state) Fiber.t)
    -> name:string
    -> Chan.t
    -> 'state
    -> 'state t

  val state : 'a t -> 'a

  val stop : _ t -> unit Fiber.t

  val stopped : _ t -> unit Fiber.t

  val run : _ t -> unit Fiber.t

  val notification : _ t -> Message.notification -> unit Fiber.t

  val request : _ t -> Message.request -> Response.t Fiber.t
end
