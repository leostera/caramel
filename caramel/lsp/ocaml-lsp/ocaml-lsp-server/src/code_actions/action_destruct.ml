open Import

let action_kind = "destruct"

let code_action_of_case_analysis uri (loc, newText) =
  let edit : WorkspaceEdit.t =
    let textedit : TextEdit.t = { range = Range.of_loc loc; newText } in
    let uri = Uri.to_string uri in
    WorkspaceEdit.create ~changes:[ (uri, [ textedit ]) ] ()
  in
  let title = String.capitalize_ascii action_kind in
  CodeAction.create ~title ~kind:(CodeActionKind.Other action_kind) ~edit
    ~isPreferred:false ()

let code_action doc (params : CodeActionParams.t) =
  let uri = Uri.t_of_yojson (`String params.textDocument.uri) in
  match Document.kind doc with
  | Intf -> Fiber.return (Ok None)
  | Impl -> (
    let command =
      let start = Position.logical params.range.start in
      let finish = Position.logical params.range.end_ in
      Query_protocol.Case_analysis (start, finish)
    in
    let open Fiber.O in
    let+ res = Document.dispatch doc command in
    match res with
    | Ok res -> Ok (Some (code_action_of_case_analysis uri res))
    | Error
        ( Merlin_analysis.Destruct.Wrong_parent _ | Query_commands.No_nodes
        | Merlin_analysis.Destruct.Not_allowed _
        | Merlin_analysis.Destruct.Useless_refine
        | Merlin_analysis.Destruct.Nothing_to_do ) ->
      Ok None
    | Error exn -> Error (Jsonrpc.Response.Error.of_exn exn))
