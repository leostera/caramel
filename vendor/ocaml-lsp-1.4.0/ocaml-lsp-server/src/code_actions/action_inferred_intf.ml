open Import

let action_kind = "inferred_intf"

let code_action_of_intf uri intf range =
  let edit : WorkspaceEdit.t =
    let textedit : TextEdit.t = { range; newText = intf } in
    let uri = Uri.to_string uri in
    WorkspaceEdit.create ~changes:[ (uri, [ textedit ]) ] ()
  in
  let title = String.capitalize_ascii "Insert inferred interface" in
  CodeAction.create ~title ~kind:(CodeActionKind.Other action_kind) ~edit
    ~isPreferred:false ()

let code_action doc (state : State.t) (params : CodeActionParams.t) =
  let open Fiber.O in
  match Document.kind doc with
  | Impl -> Fiber.return (Ok None)
  | Intf -> (
    let+ intf = Inference.infer_intf ~force_open_impl:true state doc in
    match intf with
    | Error e -> Error (Jsonrpc.Response.Error.of_exn e)
    | Ok intf ->
      Ok (Some (code_action_of_intf (Document.uri doc) intf params.range)))
