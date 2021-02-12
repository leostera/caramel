open Import

module Resolve : sig
  type t

  val uri : t -> Uri.t

  val of_completion_item : CompletionItem.t -> t

  val yojson_of_t : t -> Json.t

  val t_of_yojson : Json.t -> t
end

val complete :
     Document.t
  -> Position.t
  -> ([> `CompletionList of CompletionList.t ], Jsonrpc.Response.Error.t) result
     Fiber.t

(** creates a server response for ["completionItem/resolve"] *)
val resolve :
     Document.t
  -> CompletionItem.t
  -> Resolve.t
  -> (Document.t -> [> `Logical of int * int ] -> string option Fiber.t)
  -> markdown:bool
  -> (CompletionItem.t, Jsonrpc.Response.Error.t) result Fiber.t

val prefix_of_position :
  short_path:bool -> Msource.t -> [< Msource.position ] -> string
