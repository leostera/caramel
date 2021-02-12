open Import

val capability : string * Json.t

val meth : string

val on_request :
     params:Jsonrpc.Message.Structured.t option
  -> _
  -> (Json.t, Jsonrpc.Response.Error.t) result Fiber.t
