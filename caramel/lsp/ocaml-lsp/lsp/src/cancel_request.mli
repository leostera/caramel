open Import

val meth_ : string

include Json.Jsonable.S with type t := Jsonrpc.Id.t
