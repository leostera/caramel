module Fpath = Stdune.Path
module Ordering = Stdune.Ordering
module Json = Lsp.Import.Json
module Unix_env = Stdune.Env
module Code_error = Stdune.Code_error
module Int = Stdune.Int
module Option = Stdune.Option
module Table = Stdune.Table
module String = Stdune.String
module List = Stdune.List
module Result = Stdune.Result
module Logger = Lsp.Logger
module Loc = Location
module Scheduler = Fiber_unix.Scheduler
module Server = Lsp_fiber.Server
module Client_request = Lsp.Client_request
module Client_notification = Lsp.Client_notification
module Text_document = Lsp.Text_document
open Lsp.Types
module CompletionItemKind = CompletionItemKind
module SymbolKind = SymbolKind
module InitializeResult = InitializeResult
module SignatureHelpOptions = SignatureHelpOptions
module CodeActionOptions = CodeActionOptions
module CodeLensOptions = CodeLensOptions
module TextDocumentSyncOptions = TextDocumentSyncOptions
module TextDocumentSyncKind = TextDocumentSyncKind
module CompletionOptions = CompletionOptions
module ServerCapabilities = ServerCapabilities
module Diagnostic = Diagnostic
module PublishDiagnosticsParams = PublishDiagnosticsParams
module MessageType = MessageType
module WorkspaceEdit = WorkspaceEdit
module TextEdit = TextEdit
module CodeActionKind = CodeActionKind
module ShowMessageParams = ShowMessageParams
module ClientCapabilities = ClientCapabilities
module DiagnosticSeverity = DiagnosticSeverity
module ParameterInformation = ParameterInformation
module SignatureInformation = SignatureInformation
module SignatureHelpParams = SignatureHelpParams
module SignatureHelp = SignatureHelp
module CodeActionParams = CodeActionParams
module CodeAction = CodeAction
module CodeActionResult = CodeActionResult
module MarkupContent = MarkupContent
module MarkupKind = MarkupKind
module Hover = Hover
module Location = Location
module Command = Command
module CodeLens = CodeLens
module DocumentHighlight = DocumentHighlight
module DocumentHighlightParams = DocumentHighlightParams
module DocumentHighlightKind = DocumentHighlightKind
module DocumentSymbol = DocumentSymbol
module DocumentUri = DocumentUri
module SymbolInformation = SymbolInformation
module CompletionItem = CompletionItem
module CompletionList = CompletionList
module CompletionParams = CompletionParams
module VersionedTextDocumentIdentifier = VersionedTextDocumentIdentifier
module TextDocumentEdit = TextDocumentEdit
module FoldingRange = FoldingRange
module SelectionRange = SelectionRange
module DidOpenTextDocumentParams = DidOpenTextDocumentParams
module TextDocumentContentChangeEvent = TextDocumentContentChangeEvent
module TextDocumentIdentifier = TextDocumentIdentifier
module Server_notification = Lsp.Server_notification
module HoverParams = Lsp.Types.HoverParams
module SelectionRangeParams = Lsp.Types.SelectionRangeParams
module RenameParams = Lsp.Types.RenameParams
module CodeLensParams = Lsp.Types.CodeLensParams
module FoldingRangeParams = Lsp.Types.FoldingRangeParams
module ReferenceParams = Lsp.Types.ReferenceParams
module DidChangeConfigurationParams = Lsp.Types.DidChangeConfigurationParams
module ConfigurationParams = Lsp.Types.ConfigurationParams
module RenameOptions = Lsp.Types.RenameOptions
module Uri = Lsp.Uri
module Fiber_detached = Fiber_unix.Fiber_detached
module Io = Stdune.Io
module Reply = Lsp_fiber.Rpc.Reply

let sprintf = Stdune.sprintf

module Fiber = struct
  include Fiber

  module Result = struct
    type nonrec ('a, 'e) t = ('a, 'e) result Fiber.t

    let lift x = Fiber.map x ~f:(fun x -> Ok x)

    let return x = Fiber.return (Ok x)

    let ( >>= ) x f =
      Fiber.bind
        ~f:(function
          | Error _ as e -> Fiber.return e
          | Ok x -> f x)
        x

    module O = struct
      let ( let+ ) x f = Fiber.map ~f:(Stdune.Result.map ~f) x

      let ( let* ) x f = x >>= f
    end
  end
end

let { Logger.log } = Logger.for_section "ocaml-lsp-server"
