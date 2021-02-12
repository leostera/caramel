open! Import
open Types
open Extension

type _ t =
  | Shutdown : unit t
  | Initialize : InitializeParams.t -> InitializeResult.t t
  | TextDocumentHover : HoverParams.t -> Hover.t option t
  | TextDocumentDefinition : DefinitionParams.t -> Locations.t option t
  | TextDocumentDeclaration :
      TextDocumentPositionParams.t
      -> Locations.t option t
  | TextDocumentTypeDefinition : TypeDefinitionParams.t -> Locations.t option t
  | TextDocumentCompletion :
      CompletionParams.t
      -> [ `CompletionList of CompletionList.t
         | `List of CompletionItem.t list
         ]
         option
         t
  | TextDocumentCodeLens : CodeLensParams.t -> CodeLens.t list t
  | TextDocumentCodeLensResolve : CodeLens.t -> CodeLens.t t
  | TextDocumentPrepareRename : PrepareRenameParams.t -> Range.t option t
  | TextDocumentRename : RenameParams.t -> WorkspaceEdit.t t
  | TextDocumentLink : DocumentLinkParams.t -> DocumentLink.t list option t
  | TextDocumentLinkResolve : DocumentLink.t -> DocumentLink.t t
  | DocumentSymbol :
      DocumentSymbolParams.t
      -> [ `DocumentSymbol of DocumentSymbol.t list
         | `SymbolInformation of SymbolInformation.t list
         ]
         option
         t
  | WorkspaceSymbol :
      WorkspaceSymbolParams.t
      -> SymbolInformation.t list option t
  | DebugEcho : DebugEcho.Params.t -> DebugEcho.Result.t t
  | DebugTextDocumentGet :
      DebugTextDocumentGet.Params.t
      -> DebugTextDocumentGet.Result.t t
  | TextDocumentReferences : ReferenceParams.t -> Location.t list option t
  | TextDocumentHighlight :
      DocumentHighlightParams.t
      -> DocumentHighlight.t list option t
  | TextDocumentFoldingRange :
      FoldingRangeParams.t
      -> FoldingRange.t list option t
  | SignatureHelp : SignatureHelpParams.t -> SignatureHelp.t t
  | CodeAction : CodeActionParams.t -> CodeActionResult.t t
  | CompletionItemResolve : CompletionItem.t -> CompletionItem.t t
  | WillSaveWaitUntilTextDocument :
      WillSaveTextDocumentParams.t
      -> TextEdit.t list option t
  | TextDocumentFormatting :
      DocumentFormattingParams.t
      -> TextEdit.t list option t
  | TextDocumentOnTypeFormatting :
      DocumentOnTypeFormattingParams.t
      -> TextEdit.t list option t
  | TextDocumentColorPresentation :
      ColorPresentationParams.t
      -> ColorPresentation.t list t
  | TextDocumentColor : DocumentColorParams.t -> ColorInformation.t list t
  | SelectionRange : SelectionRangeParams.t -> SelectionRange.t list t
  | ExecuteCommand : ExecuteCommandParams.t -> Json.t t
  | UnknownRequest :
      { meth : string
      ; params : Jsonrpc.Message.Structured.t option
      }
      -> Json.t t

let yojson_of_DocumentSymbol ds : Json.t =
  Json.Option.yojson_of_t
    (function
      | `DocumentSymbol ds -> Json.To.list DocumentSymbol.yojson_of_t ds
      | `SymbolInformation si -> Json.To.list SymbolInformation.yojson_of_t si)
    ds

let yojson_of_Completion ds : Json.t =
  Json.Option.yojson_of_t
    (function
      | `CompletionList cs -> CompletionList.yojson_of_t cs
      | `List xs -> `List (List.map xs ~f:CompletionItem.yojson_of_t))
    ds

let yojson_of_result (type a) (req : a t) (result : a) =
  match (req, result) with
  | Shutdown, () -> `Null
  | Initialize _, result -> InitializeResult.yojson_of_t result
  | TextDocumentDeclaration _, result ->
    Json.Conv.yojson_of_option Locations.yojson_of_t result
  | TextDocumentHover _, result ->
    Json.Option.yojson_of_t Hover.yojson_of_t result
  | TextDocumentDefinition _, result ->
    Json.Option.yojson_of_t Locations.yojson_of_t result
  | TextDocumentTypeDefinition _, result ->
    Json.Option.yojson_of_t Locations.yojson_of_t result
  | TextDocumentCompletion _, result -> yojson_of_Completion result
  | TextDocumentCodeLens _, result -> Json.To.list CodeLens.yojson_of_t result
  | TextDocumentCodeLensResolve _, result -> CodeLens.yojson_of_t result
  | TextDocumentPrepareRename _, result ->
    Json.Option.yojson_of_t Range.yojson_of_t result
  | TextDocumentRename _, result -> WorkspaceEdit.yojson_of_t result
  | DocumentSymbol _, result -> yojson_of_DocumentSymbol result
  | DebugEcho _, result -> DebugEcho.Result.yojson_of_t result
  | DebugTextDocumentGet _, result ->
    DebugTextDocumentGet.Result.yojson_of_t result
  | TextDocumentReferences _, result ->
    Json.Option.yojson_of_t (Json.To.list Location.yojson_of_t) result
  | TextDocumentHighlight _, result ->
    Json.Option.yojson_of_t (Json.To.list DocumentHighlight.yojson_of_t) result
  | TextDocumentFoldingRange _, result ->
    Json.Option.yojson_of_t (Json.To.list FoldingRange.yojson_of_t) result
  | SignatureHelp _, result -> SignatureHelp.yojson_of_t result
  | CodeAction _, result -> CodeActionResult.yojson_of_t result
  | CompletionItemResolve _, result -> CompletionItem.yojson_of_t result
  | WillSaveWaitUntilTextDocument _, result ->
    Json.Option.yojson_of_t (Json.To.list TextEdit.yojson_of_t) result
  | TextDocumentOnTypeFormatting _, result ->
    Json.Option.yojson_of_t (Json.To.list TextEdit.yojson_of_t) result
  | TextDocumentFormatting _, result ->
    Json.Option.yojson_of_t (Json.To.list TextEdit.yojson_of_t) result
  | TextDocumentLink _, result ->
    Json.Option.yojson_of_t (Json.To.list DocumentLink.yojson_of_t) result
  | TextDocumentLinkResolve _, result -> DocumentLink.yojson_of_t result
  | WorkspaceSymbol _, result ->
    Json.Option.yojson_of_t (Json.To.list SymbolInformation.yojson_of_t) result
  | TextDocumentColorPresentation _, result ->
    Json.To.list ColorPresentation.yojson_of_t result
  | TextDocumentColor _, result ->
    Json.To.list ColorInformation.yojson_of_t result
  | SelectionRange _, result ->
    Json.yojson_of_list SelectionRange.yojson_of_t result
  | ExecuteCommand _, result -> result
  | UnknownRequest _, resp -> resp

type packed = E : 'r t -> packed

let of_jsonrpc (r : Jsonrpc.Message.request) =
  let open Result.O in
  let parse f = Jsonrpc.Message.params r f in
  match r.method_ with
  | "initialize" ->
    parse InitializeParams.t_of_yojson >>| fun params -> E (Initialize params)
  | "shutdown" -> Ok (E Shutdown)
  | "textDocument/completion" ->
    parse CompletionParams.t_of_yojson >>| fun params ->
    E (TextDocumentCompletion params)
  | "completionItem/resolve" ->
    parse CompletionItem.t_of_yojson >>| fun params ->
    E (CompletionItemResolve params)
  | "textDocument/documentSymbol" ->
    parse DocumentSymbolParams.t_of_yojson >>| fun params ->
    E (DocumentSymbol params)
  | "textDocument/hover" ->
    parse HoverParams.t_of_yojson >>| fun params -> E (TextDocumentHover params)
  | "textDocument/definition" ->
    parse DefinitionParams.t_of_yojson >>| fun params ->
    E (TextDocumentDefinition params)
  | "textDocument/typeDefinition" ->
    parse TypeDefinitionParams.t_of_yojson >>| fun params ->
    E (TextDocumentTypeDefinition params)
  | "textDocument/references" ->
    parse ReferenceParams.t_of_yojson >>| fun params ->
    E (TextDocumentReferences params)
  | "textDocument/codeLens" ->
    parse CodeLensParams.t_of_yojson >>| fun params ->
    E (TextDocumentCodeLens params)
  | "textDocument/prepareRename" ->
    parse PrepareRenameParams.t_of_yojson >>| fun params ->
    E (TextDocumentPrepareRename params)
  | "textDocument/rename" ->
    parse RenameParams.t_of_yojson >>| fun params ->
    E (TextDocumentRename params)
  | "textDocument/documentHighlight" ->
    parse DocumentHighlightParams.t_of_yojson >>| fun params ->
    E (TextDocumentHighlight params)
  | "textDocument/foldingRange" ->
    parse FoldingRangeParams.t_of_yojson >>| fun params ->
    E (TextDocumentFoldingRange params)
  | "textDocument/signatureHelp" ->
    parse SignatureHelpParams.t_of_yojson >>| fun params ->
    E (SignatureHelp params)
  | "textDocument/codeAction" ->
    parse CodeActionParams.t_of_yojson >>| fun params -> E (CodeAction params)
  | "debug/echo" ->
    parse DebugEcho.Params.t_of_yojson >>| fun params -> E (DebugEcho params)
  | "debug/textDocument/get" ->
    parse DebugTextDocumentGet.Params.t_of_yojson >>| fun params ->
    E (DebugTextDocumentGet params)
  | "textDocument/onTypeFormatting" ->
    parse DocumentOnTypeFormattingParams.t_of_yojson >>| fun params ->
    E (TextDocumentOnTypeFormatting params)
  | "textDocument/formatting" ->
    parse DocumentFormattingParams.t_of_yojson >>| fun params ->
    E (TextDocumentFormatting params)
  | "textDocument/documentLink" ->
    parse DocumentLinkParams.t_of_yojson >>| fun params ->
    E (TextDocumentLink params)
  | "textDocument/resolve" ->
    parse DocumentLink.t_of_yojson >>| fun params ->
    E (TextDocumentLinkResolve params)
  | "workspace/symbol" ->
    parse WorkspaceSymbolParams.t_of_yojson >>| fun params ->
    E (WorkspaceSymbol params)
  | "textDocument/colorPresentation" ->
    parse ColorPresentationParams.t_of_yojson >>| fun params ->
    E (TextDocumentColorPresentation params)
  | "textDocument/documentColor" ->
    parse DocumentColorParams.t_of_yojson >>| fun params ->
    E (TextDocumentColor params)
  | "textDocument/declaration" ->
    parse TextDocumentPositionParams.t_of_yojson >>| fun params ->
    E (TextDocumentDeclaration params)
  | "textDocument/selectionRange" ->
    parse SelectionRangeParams.t_of_yojson >>| fun params ->
    E (SelectionRange params)
  | "workspace/executeCommand" ->
    parse ExecuteCommandParams.t_of_yojson >>| fun params ->
    E (ExecuteCommand params)
  | meth -> Ok (E (UnknownRequest { meth; params = r.params }))

let method_ (type a) (t : a t) =
  match t with
  | Initialize _ -> "initialize"
  | ExecuteCommand _ -> "workspace/executeCommand"
  | _ -> assert false

let params (type a) (t : a t) =
  Jsonrpc.Message.Structured.of_json
    (match t with
    | Initialize params -> InitializeParams.yojson_of_t params
    | ExecuteCommand params -> ExecuteCommandParams.yojson_of_t params
    | _ -> assert false)

let to_jsonrpc_request t ~id =
  let method_ = method_ t in
  let params = params t in
  Jsonrpc.Message.create ~id ~method_ ~params ()

let response_of_json (type a) (t : a t) (json : Json.t) : a =
  match t with
  | Initialize _ -> InitializeResult.t_of_yojson json
  | ExecuteCommand _ -> json
  | _ -> assert false

let text_document (type a) (t : a t) f : TextDocumentIdentifier.t option =
  match t with
  | CompletionItemResolve _ -> None
  | TextDocumentLinkResolve _ -> None
  | ExecuteCommand _ -> None
  | TextDocumentCodeLensResolve _ -> None
  | WorkspaceSymbol _ -> None
  | DebugEcho _ -> None
  | Shutdown -> None
  | Initialize _ -> None
  | TextDocumentHover r -> Some r.textDocument
  | TextDocumentDefinition r -> Some r.textDocument
  | TextDocumentDeclaration r -> Some r.textDocument
  | TextDocumentTypeDefinition r -> Some r.textDocument
  | TextDocumentCompletion r -> Some r.textDocument
  | TextDocumentCodeLens r -> Some r.textDocument
  | TextDocumentPrepareRename r -> Some r.textDocument
  | TextDocumentRename r -> Some r.textDocument
  | TextDocumentLink r -> Some r.textDocument
  | DocumentSymbol r -> Some r.textDocument
  | DebugTextDocumentGet r -> Some r.textDocument
  | TextDocumentReferences r -> Some r.textDocument
  | TextDocumentHighlight r -> Some r.textDocument
  | TextDocumentFoldingRange r -> Some r.textDocument
  | SignatureHelp r -> Some r.textDocument
  | CodeAction r -> Some r.textDocument
  | WillSaveWaitUntilTextDocument r -> Some r.textDocument
  | TextDocumentFormatting r -> Some r.textDocument
  | TextDocumentOnTypeFormatting r -> Some r.textDocument
  | TextDocumentColorPresentation r -> Some r.textDocument
  | TextDocumentColor r -> Some r.textDocument
  | SelectionRange r -> Some r.textDocument
  | UnknownRequest { meth; params } -> f ~meth ~params
