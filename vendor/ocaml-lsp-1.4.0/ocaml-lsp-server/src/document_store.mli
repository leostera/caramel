open Import

type t

val make : unit -> t

val put : t -> Document.t -> unit

val get : t -> Uri.t -> (Document.t, Jsonrpc.Response.Error.t) result

val get_opt : t -> Uri.t -> Document.t option

val remove_document : t -> Uri.t -> unit Fiber.t

val get_size : t -> int

val close : t -> unit Fiber.t
