(** Low level module for sending/receiving jsonrpc packets across channels *)
type t

val make : in_channel -> out_channel -> t

val read : t -> Jsonrpc.packet option

val send : t -> Jsonrpc.packet -> unit

val close_in : t -> unit

val close_out : t -> unit

val close : t -> unit
