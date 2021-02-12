open! Import
open Jsonrpc

module Reply : sig
  type 'resp t

  val now : ('r, Jsonrpc.Response.Error.t) result -> 'r t

  val later :
       ((('r, Jsonrpc.Response.Error.t) result -> unit Fiber.t) -> unit Fiber.t)
    -> 'r t
end

module type S = sig
  type 'a out_request

  type out_notification

  type 'a in_request

  type in_notification

  type 'state t

  module Handler : sig
    type 'a session

    type 'state on_request =
      { on_request :
          'a. 'state session -> 'a in_request -> ('a Reply.t * 'state) Fiber.t
      }

    type 'state t

    val make :
         ?on_request:'state on_request
      -> ?on_notification:('state session -> in_notification -> 'state Fiber.t)
      -> unit
      -> 'state t
  end
  with type 'a session := 'a t

  val state : 'a t -> 'a

  val make : 'state Handler.t -> Fiber_io.t -> 'state -> 'state t

  val stop : 'state t -> unit Fiber.t

  val request :
    _ t -> 'resp out_request -> ('resp, Response.Error.t) result Fiber.t

  val notification : _ t -> out_notification -> unit Fiber.t

  val on_cancel : (unit -> unit Fiber.t) -> unit
end

module Client : sig
  open Types

  include
    S
      with type 'a out_request = 'a Client_request.t
       and type out_notification = Client_notification.t
       and type 'a in_request = 'a Server_request.t
       and type in_notification = Server_notification.t

  val initialized : _ t -> InitializeResult.t Fiber.t

  val start : _ t -> InitializeParams.t -> unit Fiber.t
end

module Server : sig
  open Types

  include
    S
      with type 'a out_request = 'a Server_request.t
       and type out_notification = Server_notification.t
       and type 'a in_request = 'a Client_request.t
       and type in_notification = Client_notification.t

  val initialized : _ t -> InitializeParams.t Fiber.t

  val start : _ t -> unit Fiber.t
end
