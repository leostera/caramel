open Import
open Types

type t =
  | TextDocumentDidOpen of DidOpenTextDocumentParams.t
  | TextDocumentDidClose of DidCloseTextDocumentParams.t
  | TextDocumentDidChange of DidChangeTextDocumentParams.t
  | DidSaveTextDocument of DidSaveTextDocumentParams.t
  | WillSaveTextDocument of WillSaveTextDocumentParams.t
  | ChangeWorkspaceFolders of DidChangeWorkspaceFoldersParams.t
  | ChangeConfiguration of DidChangeConfigurationParams.t
  | Initialized
  | Exit
  | CancelRequest of Jsonrpc.Id.t
  | Unknown_notification of Jsonrpc.Message.notification

let method_ = function
  | TextDocumentDidOpen _ -> "textDocument/didOpen"
  | TextDocumentDidChange _ -> "textDocument/didChange"
  | TextDocumentDidClose _ -> "textDocument/didClose"
  | Exit -> "exit"
  | Initialized -> "initialized"
  | ChangeWorkspaceFolders _ -> "workspace/didChangeWorkspaceFolders"
  | ChangeConfiguration _ -> "workspace/didChangeConfiguration"
  | WillSaveTextDocument _ -> "textDocument/willSave"
  | DidSaveTextDocument _ -> "textDocument/didSave"
  | CancelRequest _ -> Cancel_request.meth_
  | Unknown_notification _ -> assert false

let yojson_of_t = function
  | TextDocumentDidOpen params -> DidOpenTextDocumentParams.yojson_of_t params
  | TextDocumentDidChange params ->
    DidChangeTextDocumentParams.yojson_of_t params
  | TextDocumentDidClose params -> DidCloseTextDocumentParams.yojson_of_t params
  | Exit -> `Null
  | Initialized -> `Null
  | ChangeWorkspaceFolders params ->
    DidChangeWorkspaceFoldersParams.yojson_of_t params
  | ChangeConfiguration params ->
    DidChangeConfigurationParams.yojson_of_t params
  | WillSaveTextDocument params -> WillSaveTextDocumentParams.yojson_of_t params
  | DidSaveTextDocument params -> DidSaveTextDocumentParams.yojson_of_t params
  | CancelRequest params -> Cancel_request.yojson_of_t params
  | Unknown_notification _ -> assert false

let of_jsonrpc (r : Jsonrpc.Message.notification) =
  let open Result.O in
  match r.method_ with
  | "textDocument/didOpen" ->
    Jsonrpc.Message.params r DidOpenTextDocumentParams.t_of_yojson
    >>| fun params -> TextDocumentDidOpen params
  | "textDocument/didChange" ->
    Jsonrpc.Message.params r DidChangeTextDocumentParams.t_of_yojson
    >>| fun params -> TextDocumentDidChange params
  | "textDocument/didClose" ->
    Jsonrpc.Message.params r DidCloseTextDocumentParams.t_of_yojson
    >>| fun params -> TextDocumentDidClose params
  | "exit" -> Ok Exit
  | "initialized" -> Ok Initialized
  | "workspace/didChangeWorkspaceFolders" ->
    Jsonrpc.Message.params r DidChangeWorkspaceFoldersParams.t_of_yojson
    >>| fun params -> ChangeWorkspaceFolders params
  | "workspace/didChangeConfiguration" ->
    Jsonrpc.Message.params r DidChangeConfigurationParams.t_of_yojson
    >>| fun params -> ChangeConfiguration params
  | "textDocument/willSave" ->
    Jsonrpc.Message.params r WillSaveTextDocumentParams.t_of_yojson
    >>| fun params -> WillSaveTextDocument params
  | "textDocument/didSave" ->
    Jsonrpc.Message.params r DidSaveTextDocumentParams.t_of_yojson
    >>| fun params -> DidSaveTextDocument params
  | m when m = Cancel_request.meth_ ->
    let+ params = Jsonrpc.Message.params r Cancel_request.t_of_yojson in
    CancelRequest params
  | _ -> Ok (Unknown_notification r)

let to_jsonrpc t =
  let method_ = method_ t in
  let params = Some (Jsonrpc.Message.Structured.of_json (yojson_of_t t)) in
  { Jsonrpc.Message.id = (); params; method_ }
