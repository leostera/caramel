open Import

let capability = ("handleSwitchImplIntf", `Bool true)

let meth = "ocamllsp/switchImplIntf"

(** see the spec for [ocamllsp/switchImplIntf] *)
let switch (param : DocumentUri.t) : (Json.t, Jsonrpc.Response.Error.t) result =
  let files_to_switch_to =
    Document.get_impl_intf_counterparts (Uri.t_of_yojson (`String param))
  in
  Ok (Json.yojson_of_list Uri.yojson_of_t files_to_switch_to)

let on_request ~(params : Jsonrpc.Message.Structured.t option) _ =
  Fiber.return
    (match params with
    | Some (`List [ `String (file_uri : DocumentUri.t) ]) -> switch file_uri
    | Some json ->
      Error
        (Jsonrpc.Response.Error.make ~code:InvalidRequest
           ~message:"The input parameter for ocamllsp/switchImplIntf is invalid"
           ~data:(`Assoc [ ("param", (json :> Json.t)) ])
           ())
    | None ->
      Error
        (Jsonrpc.Response.Error.make ~code:InvalidRequest
           ~message:"ocamllsp/switchImplIntf must receive param: DocumentUri.t"
           ()))
