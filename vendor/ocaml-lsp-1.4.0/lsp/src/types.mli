open! Import

module MarkedString : sig
  type t =
    { value : string
    ; language : string option
    }

  include Json.Jsonable.S with type t := t
end

(*$ Lsp_gen.print_mli () *)

module DeleteFileOptions : sig
  type t =
    { recursive : bool option
    ; ignoreIfNotExists : bool option
    }

  include Json.Jsonable.S with type t := t

  val create : ?recursive:bool -> ?ignoreIfNotExists:bool -> unit -> t
end

module DocumentUri : sig
  type t = string

  include Json.Jsonable.S with type t := t
end

module DeleteFile : sig
  type t =
    { uri : DocumentUri.t
    ; options : DeleteFileOptions.t option
    }

  include Json.Jsonable.S with type t := t

  val create : uri:DocumentUri.t -> ?options:DeleteFileOptions.t -> unit -> t
end

module RenameFileOptions : sig
  type t =
    { overwrite : bool option
    ; ignoreIfExists : bool option
    }

  include Json.Jsonable.S with type t := t

  val create : ?overwrite:bool -> ?ignoreIfExists:bool -> unit -> t
end

module RenameFile : sig
  type t =
    { oldUri : DocumentUri.t
    ; newUri : DocumentUri.t
    ; options : RenameFileOptions.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       oldUri:DocumentUri.t
    -> newUri:DocumentUri.t
    -> ?options:RenameFileOptions.t
    -> unit
    -> t
end

module CreateFileOptions : sig
  type t =
    { overwrite : bool option
    ; ignoreIfExists : bool option
    }

  include Json.Jsonable.S with type t := t

  val create : ?overwrite:bool -> ?ignoreIfExists:bool -> unit -> t
end

module CreateFile : sig
  type t =
    { uri : DocumentUri.t
    ; options : CreateFileOptions.t option
    }

  include Json.Jsonable.S with type t := t

  val create : uri:DocumentUri.t -> ?options:CreateFileOptions.t -> unit -> t
end

module Position : sig
  type t =
    { line : int
    ; character : int
    }

  include Json.Jsonable.S with type t := t

  val create : line:int -> character:int -> t
end

module Range : sig
  type t =
    { start : Position.t
    ; end_ : Position.t
    }

  include Json.Jsonable.S with type t := t

  val create : start:Position.t -> end_:Position.t -> t
end

module TextEdit : sig
  type t =
    { range : Range.t
    ; newText : string
    }

  include Json.Jsonable.S with type t := t

  val create : range:Range.t -> newText:string -> t
end

module TextDocumentIdentifier : sig
  type t = { uri : DocumentUri.t }

  include Json.Jsonable.S with type t := t

  val create : uri:DocumentUri.t -> t
end

module VersionedTextDocumentIdentifier : sig
  type t =
    { uri : DocumentUri.t
    ; version : int option
    }

  include Json.Jsonable.S with type t := t

  val create : uri:DocumentUri.t -> ?version:int -> unit -> t
end

module TextDocumentEdit : sig
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; edits : TextEdit.t list
    }

  include Json.Jsonable.S with type t := t

  val create :
    textDocument:VersionedTextDocumentIdentifier.t -> edits:TextEdit.t list -> t
end

module WorkspaceEdit : sig
  type t =
    { changes : (DocumentUri.t * TextEdit.t list) list option
    ; documentChanges :
        [ `TextDocumentEdit of TextDocumentEdit.t
        | `CreateFile of CreateFile.t
        | `RenameFile of RenameFile.t
        | `DeleteFile of DeleteFile.t
        ]
        list
        option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?changes:(DocumentUri.t * TextEdit.t list) list
    -> ?documentChanges:
         [ `TextDocumentEdit of TextDocumentEdit.t
         | `CreateFile of CreateFile.t
         | `RenameFile of RenameFile.t
         | `DeleteFile of DeleteFile.t
         ]
         list
    -> unit
    -> t
end

module ApplyWorkspaceEditParams : sig
  type t =
    { label : string option
    ; edit : WorkspaceEdit.t
    }

  include Json.Jsonable.S with type t := t

  val create : ?label:string -> edit:WorkspaceEdit.t -> unit -> t
end

module ApplyWorkspaceEditResponse : sig
  type t =
    { applied : bool
    ; failureReason : string option
    }

  include Json.Jsonable.S with type t := t

  val create : applied:bool -> ?failureReason:string -> unit -> t
end

module CancelParams : sig
  type t = { id : Jsonrpc.Id.t }

  include Json.Jsonable.S with type t := t

  val create : id:Jsonrpc.Id.t -> t
end

module SelectionRangeClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> unit -> t
end

module FoldingRangeClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; rangeLimit : int option
    ; lineFoldingOnly : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?dynamicRegistration:bool
    -> ?rangeLimit:int
    -> ?lineFoldingOnly:bool
    -> unit
    -> t
end

module DiagnosticTag : sig
  type t =
    | Unnecessary
    | Deprecated

  include Json.Jsonable.S with type t := t
end

module PublishDiagnosticsClientCapabilities : sig
  type tagSupport = { valueSet : DiagnosticTag.t list }

  include Json.Jsonable.S with type t := tagSupport

  val create_tagSupport : valueSet:DiagnosticTag.t list -> tagSupport

  type t =
    { relatedInformation : bool option
    ; tagSupport : tagSupport option
    ; versionSupport : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?relatedInformation:bool
    -> ?tagSupport:tagSupport
    -> ?versionSupport:bool
    -> unit
    -> t
end

module RenameClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; prepareSupport : bool option
    }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> ?prepareSupport:bool -> unit -> t
end

module DocumentOnTypeFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> unit -> t
end

module DocumentRangeFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> unit -> t
end

module DocumentFormattingClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> unit -> t
end

module DocumentColorClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> unit -> t
end

module DocumentLinkClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; tooltipSupport : bool option
    }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> ?tooltipSupport:bool -> unit -> t
end

module CodeLensClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> unit -> t
end

module CodeActionKind : sig
  type t =
    | Empty
    | QuickFix
    | Refactor
    | RefactorExtract
    | RefactorInline
    | RefactorRewrite
    | Source
    | SourceOrganizeImports
    | Other of string

  include Json.Jsonable.S with type t := t
end

module CodeActionClientCapabilities : sig
  type codeActionKind = { valueSet : CodeActionKind.t list }

  include Json.Jsonable.S with type t := codeActionKind

  val create_codeActionKind : valueSet:CodeActionKind.t list -> codeActionKind

  type codeActionLiteralSupport = { codeActionKind : codeActionKind }

  include Json.Jsonable.S with type t := codeActionLiteralSupport

  val create_codeActionLiteralSupport :
    codeActionKind:codeActionKind -> codeActionLiteralSupport

  type t =
    { dynamicRegistration : bool option
    ; codeActionLiteralSupport : codeActionLiteralSupport option
    ; isPreferredSupport : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?dynamicRegistration:bool
    -> ?codeActionLiteralSupport:codeActionLiteralSupport
    -> ?isPreferredSupport:bool
    -> unit
    -> t
end

module SymbolKind : sig
  type t =
    | File
    | Module
    | Namespace
    | Package
    | Class
    | Method
    | Property
    | Field
    | Constructor
    | Enum
    | Interface
    | Function
    | Variable
    | Constant
    | String
    | Number
    | Boolean
    | Array
    | Object
    | Key
    | Null
    | EnumMember
    | Struct
    | Event
    | Operator
    | TypeParameter

  include Json.Jsonable.S with type t := t
end

module DocumentSymbolClientCapabilities : sig
  type symbolKind = { valueSet : SymbolKind.t list option }

  include Json.Jsonable.S with type t := symbolKind

  val create_symbolKind : ?valueSet:SymbolKind.t list -> unit -> symbolKind

  type t =
    { dynamicRegistration : bool option
    ; symbolKind : symbolKind option
    ; hierarchicalDocumentSymbolSupport : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?dynamicRegistration:bool
    -> ?symbolKind:symbolKind
    -> ?hierarchicalDocumentSymbolSupport:bool
    -> unit
    -> t
end

module DocumentHighlightClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> unit -> t
end

module ReferenceClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> unit -> t
end

module ImplementationClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; linkSupport : bool option
    }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> ?linkSupport:bool -> unit -> t
end

module TypeDefinitionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; linkSupport : bool option
    }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> ?linkSupport:bool -> unit -> t
end

module DefinitionClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; linkSupport : bool option
    }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> ?linkSupport:bool -> unit -> t
end

module DeclarationClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; linkSupport : bool option
    }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> ?linkSupport:bool -> unit -> t
end

module MarkupKind : sig
  type t =
    | PlainText
    | Markdown

  include Json.Jsonable.S with type t := t
end

module SignatureHelpClientCapabilities : sig
  type parameterInformation = { labelOffsetSupport : bool option }

  include Json.Jsonable.S with type t := parameterInformation

  val create_parameterInformation :
    ?labelOffsetSupport:bool -> unit -> parameterInformation

  type signatureInformation =
    { documentationFormat : MarkupKind.t list option
    ; parameterInformation : parameterInformation option
    }

  include Json.Jsonable.S with type t := signatureInformation

  val create_signatureInformation :
       ?documentationFormat:MarkupKind.t list
    -> ?parameterInformation:parameterInformation
    -> unit
    -> signatureInformation

  type t =
    { dynamicRegistration : bool option
    ; signatureInformation : signatureInformation option
    ; contextSupport : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?dynamicRegistration:bool
    -> ?signatureInformation:signatureInformation
    -> ?contextSupport:bool
    -> unit
    -> t
end

module HoverClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; contentFormat : MarkupKind.t list option
    }

  include Json.Jsonable.S with type t := t

  val create :
    ?dynamicRegistration:bool -> ?contentFormat:MarkupKind.t list -> unit -> t
end

module CompletionItemKind : sig
  type t =
    | Text
    | Method
    | Function
    | Constructor
    | Field
    | Variable
    | Class
    | Interface
    | Module
    | Property
    | Unit
    | Value
    | Enum
    | Keyword
    | Snippet
    | Color
    | File
    | Reference
    | Folder
    | EnumMember
    | Constant
    | Struct
    | Event
    | Operator
    | TypeParameter

  include Json.Jsonable.S with type t := t
end

module CompletionItemTag : sig
  type t = Deprecated

  include Json.Jsonable.S with type t := t
end

module CompletionClientCapabilities : sig
  type completionItemKind = { valueSet : CompletionItemKind.t list option }

  include Json.Jsonable.S with type t := completionItemKind

  val create_completionItemKind :
    ?valueSet:CompletionItemKind.t list -> unit -> completionItemKind

  type tagSupport = { valueSet : CompletionItemTag.t list }

  include Json.Jsonable.S with type t := tagSupport

  val create_tagSupport : valueSet:CompletionItemTag.t list -> tagSupport

  type completionItem =
    { snippetSupport : bool option
    ; commitCharactersSupport : bool option
    ; documentationFormat : MarkupKind.t list option
    ; deprecatedSupport : bool option
    ; preselectSupport : bool option
    ; tagSupport : tagSupport option
    }

  include Json.Jsonable.S with type t := completionItem

  val create_completionItem :
       ?snippetSupport:bool
    -> ?commitCharactersSupport:bool
    -> ?documentationFormat:MarkupKind.t list
    -> ?deprecatedSupport:bool
    -> ?preselectSupport:bool
    -> ?tagSupport:tagSupport
    -> unit
    -> completionItem

  type t =
    { dynamicRegistration : bool option
    ; completionItem : completionItem option
    ; completionItemKind : completionItemKind option
    ; contextSupport : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?dynamicRegistration:bool
    -> ?completionItem:completionItem
    -> ?completionItemKind:completionItemKind
    -> ?contextSupport:bool
    -> unit
    -> t
end

module TextDocumentSyncClientCapabilities : sig
  type t =
    { dynamicRegistration : bool option
    ; willSave : bool option
    ; willSaveWaitUntil : bool option
    ; didSave : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?dynamicRegistration:bool
    -> ?willSave:bool
    -> ?willSaveWaitUntil:bool
    -> ?didSave:bool
    -> unit
    -> t
end

module TextDocumentClientCapabilities : sig
  type t =
    { synchronization : TextDocumentSyncClientCapabilities.t option
    ; completion : CompletionClientCapabilities.t option
    ; hover : HoverClientCapabilities.t option
    ; signatureHelp : SignatureHelpClientCapabilities.t option
    ; declaration : DeclarationClientCapabilities.t option
    ; definition : DefinitionClientCapabilities.t option
    ; typeDefinition : TypeDefinitionClientCapabilities.t option
    ; implementation : ImplementationClientCapabilities.t option
    ; references : ReferenceClientCapabilities.t option
    ; documentHighlight : DocumentHighlightClientCapabilities.t option
    ; documentSymbol : DocumentSymbolClientCapabilities.t option
    ; codeAction : CodeActionClientCapabilities.t option
    ; codeLens : CodeLensClientCapabilities.t option
    ; documentLink : DocumentLinkClientCapabilities.t option
    ; colorProvider : DocumentColorClientCapabilities.t option
    ; formatting : DocumentFormattingClientCapabilities.t option
    ; rangeFormatting : DocumentRangeFormattingClientCapabilities.t option
    ; onTypeFormatting : DocumentOnTypeFormattingClientCapabilities.t option
    ; rename : RenameClientCapabilities.t option
    ; publishDiagnostics : PublishDiagnosticsClientCapabilities.t option
    ; foldingRange : FoldingRangeClientCapabilities.t option
    ; selectionRange : SelectionRangeClientCapabilities.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?synchronization:TextDocumentSyncClientCapabilities.t
    -> ?completion:CompletionClientCapabilities.t
    -> ?hover:HoverClientCapabilities.t
    -> ?signatureHelp:SignatureHelpClientCapabilities.t
    -> ?declaration:DeclarationClientCapabilities.t
    -> ?definition:DefinitionClientCapabilities.t
    -> ?typeDefinition:TypeDefinitionClientCapabilities.t
    -> ?implementation:ImplementationClientCapabilities.t
    -> ?references:ReferenceClientCapabilities.t
    -> ?documentHighlight:DocumentHighlightClientCapabilities.t
    -> ?documentSymbol:DocumentSymbolClientCapabilities.t
    -> ?codeAction:CodeActionClientCapabilities.t
    -> ?codeLens:CodeLensClientCapabilities.t
    -> ?documentLink:DocumentLinkClientCapabilities.t
    -> ?colorProvider:DocumentColorClientCapabilities.t
    -> ?formatting:DocumentFormattingClientCapabilities.t
    -> ?rangeFormatting:DocumentRangeFormattingClientCapabilities.t
    -> ?onTypeFormatting:DocumentOnTypeFormattingClientCapabilities.t
    -> ?rename:RenameClientCapabilities.t
    -> ?publishDiagnostics:PublishDiagnosticsClientCapabilities.t
    -> ?foldingRange:FoldingRangeClientCapabilities.t
    -> ?selectionRange:SelectionRangeClientCapabilities.t
    -> unit
    -> t
end

module ExecuteCommandClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> unit -> t
end

module WorkspaceSymbolClientCapabilities : sig
  type symbolKind = { valueSet : SymbolKind.t list option }

  include Json.Jsonable.S with type t := symbolKind

  val create_symbolKind : ?valueSet:SymbolKind.t list -> unit -> symbolKind

  type t =
    { dynamicRegistration : bool option
    ; symbolKind : symbolKind option
    }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> ?symbolKind:symbolKind -> unit -> t
end

module DidChangeWatchedFilesClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> unit -> t
end

module DidChangeConfigurationClientCapabilities : sig
  type t = { dynamicRegistration : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?dynamicRegistration:bool -> unit -> t
end

module FailureHandlingKind : sig
  type t =
    | Abort
    | Transactional
    | TextOnlyTransactional
    | Undo

  include Json.Jsonable.S with type t := t
end

module ResourceOperationKind : sig
  type t =
    | Create
    | Rename
    | Delete

  include Json.Jsonable.S with type t := t
end

module WorkspaceEditClientCapabilities : sig
  type t =
    { documentChanges : bool option
    ; resourceOperations : ResourceOperationKind.t list option
    ; failureHandling : FailureHandlingKind.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?documentChanges:bool
    -> ?resourceOperations:ResourceOperationKind.t list
    -> ?failureHandling:FailureHandlingKind.t
    -> unit
    -> t
end

module ClientCapabilities : sig
  type window = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := window

  val create_window : ?workDoneProgress:bool -> unit -> window

  type workspace =
    { applyEdit : bool option
    ; workspaceEdit : WorkspaceEditClientCapabilities.t option
    ; didChangeConfiguration : DidChangeConfigurationClientCapabilities.t option
    ; didChangeWatchedFiles : DidChangeWatchedFilesClientCapabilities.t option
    ; symbol : WorkspaceSymbolClientCapabilities.t option
    ; executeCommand : ExecuteCommandClientCapabilities.t option
    ; workspaceFolders : bool option
    ; configuration : bool option
    }

  include Json.Jsonable.S with type t := workspace

  val create_workspace :
       ?applyEdit:bool
    -> ?workspaceEdit:WorkspaceEditClientCapabilities.t
    -> ?didChangeConfiguration:DidChangeConfigurationClientCapabilities.t
    -> ?didChangeWatchedFiles:DidChangeWatchedFilesClientCapabilities.t
    -> ?symbol:WorkspaceSymbolClientCapabilities.t
    -> ?executeCommand:ExecuteCommandClientCapabilities.t
    -> ?workspaceFolders:bool
    -> ?configuration:bool
    -> unit
    -> workspace

  type t =
    { workspace : workspace option
    ; textDocument : TextDocumentClientCapabilities.t option
    ; window : window option
    ; experimental : Json.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?workspace:workspace
    -> ?textDocument:TextDocumentClientCapabilities.t
    -> ?window:window
    -> ?experimental:Json.t
    -> unit
    -> t
end

module Command : sig
  type t =
    { title : string
    ; command : string
    ; arguments : Json.t list option
    }

  include Json.Jsonable.S with type t := t

  val create :
    title:string -> command:string -> ?arguments:Json.t list -> unit -> t
end

module Location : sig
  type t =
    { uri : DocumentUri.t
    ; range : Range.t
    }

  include Json.Jsonable.S with type t := t

  val create : uri:DocumentUri.t -> range:Range.t -> t
end

module DiagnosticRelatedInformation : sig
  type t =
    { location : Location.t
    ; message : string
    }

  include Json.Jsonable.S with type t := t

  val create : location:Location.t -> message:string -> t
end

module DiagnosticSeverity : sig
  type t =
    | Error
    | Warning
    | Information
    | Hint

  include Json.Jsonable.S with type t := t
end

module Diagnostic : sig
  type t =
    { range : Range.t
    ; severity : DiagnosticSeverity.t option
    ; code : Jsonrpc.Id.t option
    ; source : string option
    ; message : string
    ; tags : DiagnosticTag.t list option
    ; relatedInformation : DiagnosticRelatedInformation.t list option
    }

  include Json.Jsonable.S with type t := t

  val create :
       range:Range.t
    -> ?severity:DiagnosticSeverity.t
    -> ?code:Jsonrpc.Id.t
    -> ?source:string
    -> message:string
    -> ?tags:DiagnosticTag.t list
    -> ?relatedInformation:DiagnosticRelatedInformation.t list
    -> unit
    -> t
end

module CodeAction : sig
  type t =
    { title : string
    ; kind : CodeActionKind.t option
    ; diagnostics : Diagnostic.t list option
    ; isPreferred : bool option
    ; edit : WorkspaceEdit.t option
    ; command : Command.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       title:string
    -> ?kind:CodeActionKind.t
    -> ?diagnostics:Diagnostic.t list
    -> ?isPreferred:bool
    -> ?edit:WorkspaceEdit.t
    -> ?command:Command.t
    -> unit
    -> t
end

module CodeActionContext : sig
  type t =
    { diagnostics : Diagnostic.t list
    ; only : CodeActionKind.t list option
    }

  include Json.Jsonable.S with type t := t

  val create :
    diagnostics:Diagnostic.t list -> ?only:CodeActionKind.t list -> unit -> t
end

module WorkDoneProgressOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module CodeActionOptions : sig
  type t =
    { workDoneProgress : bool option
    ; codeActionKinds : CodeActionKind.t list option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?workDoneProgress:bool
    -> ?codeActionKinds:CodeActionKind.t list
    -> unit
    -> t
end

module ProgressToken : sig
  type t = Jsonrpc.Id.t

  include Json.Jsonable.S with type t := t
end

module PartialResultParams : sig
  type t = { partialResultToken : ProgressToken.t option }

  include Json.Jsonable.S with type t := t

  val create : ?partialResultToken:ProgressToken.t -> unit -> t
end

module WorkDoneProgressParams : sig
  type t = { workDoneToken : ProgressToken.t option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneToken:ProgressToken.t -> unit -> t
end

module CodeActionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : CodeActionContext.t
    }

  include Json.Jsonable.S with type t := t

  val create :
       textDocument:TextDocumentIdentifier.t
    -> range:Range.t
    -> context:CodeActionContext.t
    -> t
end

module DocumentFilter : sig
  type t =
    { language : string option
    ; scheme : string option
    ; pattern : string option
    }

  include Json.Jsonable.S with type t := t

  val create :
    ?language:string -> ?scheme:string -> ?pattern:string -> unit -> t
end

module DocumentSelector : sig
  type t = DocumentFilter.t list

  include Json.Jsonable.S with type t := t
end

module TextDocumentRegistrationOptions : sig
  type t = { documentSelector : DocumentSelector.t option }

  include Json.Jsonable.S with type t := t

  val create : ?documentSelector:DocumentSelector.t -> unit -> t
end

module CodeActionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; codeActionKinds : CodeActionKind.t list option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?documentSelector:DocumentSelector.t
    -> ?workDoneProgress:bool
    -> ?codeActionKinds:CodeActionKind.t list
    -> unit
    -> t
end

module CodeLens : sig
  type t =
    { range : Range.t
    ; command : Command.t option
    ; data : Json.t option
    }

  include Json.Jsonable.S with type t := t

  val create : range:Range.t -> ?command:Command.t -> ?data:Json.t -> unit -> t
end

module CodeLensOptions : sig
  type t =
    { workDoneProgress : bool option
    ; resolveProvider : bool option
    }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> ?resolveProvider:bool -> unit -> t
end

module CodeLensParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> t
end

module CodeLensRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; resolveProvider : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?documentSelector:DocumentSelector.t
    -> ?workDoneProgress:bool
    -> ?resolveProvider:bool
    -> unit
    -> t
end

module Color : sig
  type t =
    { red : int
    ; green : int
    ; blue : int
    ; alpha : int
    }

  include Json.Jsonable.S with type t := t

  val create : red:int -> green:int -> blue:int -> alpha:int -> t
end

module ColorInformation : sig
  type t =
    { range : Range.t
    ; color : Color.t
    }

  include Json.Jsonable.S with type t := t

  val create : range:Range.t -> color:Color.t -> t
end

module ColorPresentation : sig
  type t =
    { label : string
    ; textEdit : TextEdit.t option
    ; additionalTextEdits : TextEdit.t list option
    }

  include Json.Jsonable.S with type t := t

  val create :
       label:string
    -> ?textEdit:TextEdit.t
    -> ?additionalTextEdits:TextEdit.t list
    -> unit
    -> t
end

module ColorPresentationParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; color : Color.t
    ; range : Range.t
    }

  include Json.Jsonable.S with type t := t

  val create :
    textDocument:TextDocumentIdentifier.t -> color:Color.t -> range:Range.t -> t
end

module CompletionTriggerKind : sig
  type t =
    | Invoked
    | TriggerCharacter
    | TriggerForIncompleteCompletions

  include Json.Jsonable.S with type t := t
end

module CompletionContext : sig
  type t =
    { triggerKind : CompletionTriggerKind.t
    ; triggerCharacter : string option
    }

  include Json.Jsonable.S with type t := t

  val create :
    triggerKind:CompletionTriggerKind.t -> ?triggerCharacter:string -> unit -> t
end

module InsertTextFormat : sig
  type t =
    | PlainText
    | Snippet

  include Json.Jsonable.S with type t := t
end

module MarkupContent : sig
  type t =
    { kind : MarkupKind.t
    ; value : string
    }

  include Json.Jsonable.S with type t := t

  val create : kind:MarkupKind.t -> value:string -> t
end

module CompletionItem : sig
  type t =
    { label : string
    ; kind : CompletionItemKind.t option
    ; tags : CompletionItemTag.t list option
    ; detail : string option
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ] option
    ; deprecated : bool option
    ; preselect : bool option
    ; sortText : string option
    ; filterText : string option
    ; insertText : string option
    ; insertTextFormat : InsertTextFormat.t option
    ; textEdit : TextEdit.t option
    ; additionalTextEdits : TextEdit.t list option
    ; commitCharacters : string list option
    ; command : Command.t option
    ; data : Json.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       label:string
    -> ?kind:CompletionItemKind.t
    -> ?tags:CompletionItemTag.t list
    -> ?detail:string
    -> ?documentation:[ `String of string | `MarkupContent of MarkupContent.t ]
    -> ?deprecated:bool
    -> ?preselect:bool
    -> ?sortText:string
    -> ?filterText:string
    -> ?insertText:string
    -> ?insertTextFormat:InsertTextFormat.t
    -> ?textEdit:TextEdit.t
    -> ?additionalTextEdits:TextEdit.t list
    -> ?commitCharacters:string list
    -> ?command:Command.t
    -> ?data:Json.t
    -> unit
    -> t
end

module CompletionList : sig
  type t =
    { isIncomplete : bool
    ; items : CompletionItem.t list
    }

  include Json.Jsonable.S with type t := t

  val create : isIncomplete:bool -> items:CompletionItem.t list -> t
end

module CompletionOptions : sig
  type t =
    { workDoneProgress : bool option
    ; triggerCharacters : string list option
    ; allCommitCharacters : string list option
    ; resolveProvider : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?workDoneProgress:bool
    -> ?triggerCharacters:string list
    -> ?allCommitCharacters:string list
    -> ?resolveProvider:bool
    -> unit
    -> t
end

module TextDocumentPositionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> position:Position.t -> t
end

module CompletionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; context : CompletionContext.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       textDocument:TextDocumentIdentifier.t
    -> position:Position.t
    -> ?context:CompletionContext.t
    -> unit
    -> t
end

module CompletionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; triggerCharacters : string list option
    ; allCommitCharacters : string list option
    ; resolveProvider : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?documentSelector:DocumentSelector.t
    -> ?workDoneProgress:bool
    -> ?triggerCharacters:string list
    -> ?allCommitCharacters:string list
    -> ?resolveProvider:bool
    -> unit
    -> t
end

module ConfigurationItem : sig
  type t =
    { scopeUri : DocumentUri.t option
    ; section : string option
    }

  include Json.Jsonable.S with type t := t

  val create : ?scopeUri:DocumentUri.t -> ?section:string -> unit -> t
end

module ConfigurationParams : sig
  type t = { items : ConfigurationItem.t list }

  include Json.Jsonable.S with type t := t

  val create : items:ConfigurationItem.t list -> t
end

module DeclarationOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module DeclarationParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> position:Position.t -> t
end

module StaticRegistrationOptions : sig
  type t = { id : string option }

  include Json.Jsonable.S with type t := t

  val create : ?id:string -> unit -> t
end

module DeclarationRegistrationOptions : sig
  type t =
    { workDoneProgress : bool option
    ; documentSelector : DocumentSelector.t option
    ; id : string option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?workDoneProgress:bool
    -> ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> unit
    -> t
end

module DefinitionOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module DefinitionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> position:Position.t -> t
end

module DefinitionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
    ?documentSelector:DocumentSelector.t -> ?workDoneProgress:bool -> unit -> t
end

module DidChangeConfigurationParams : sig
  type t = { settings : Json.t }

  include Json.Jsonable.S with type t := t

  val create : settings:Json.t -> t
end

module TextDocumentContentChangeEvent : sig
  type t =
    { range : Range.t option
    ; rangeLength : int option
    ; text : string
    }

  include Json.Jsonable.S with type t := t

  val create : ?range:Range.t -> ?rangeLength:int -> text:string -> unit -> t
end

module DidChangeTextDocumentParams : sig
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; contentChanges : TextDocumentContentChangeEvent.t list
    }

  include Json.Jsonable.S with type t := t

  val create :
       textDocument:VersionedTextDocumentIdentifier.t
    -> contentChanges:TextDocumentContentChangeEvent.t list
    -> t
end

module FileEvent : sig
  type t =
    { uri : DocumentUri.t
    ; type_ : int
    }

  include Json.Jsonable.S with type t := t

  val create : uri:DocumentUri.t -> type_:int -> t
end

module DidChangeWatchedFilesParams : sig
  type t = { changes : FileEvent.t list }

  include Json.Jsonable.S with type t := t

  val create : changes:FileEvent.t list -> t
end

module FileSystemWatcher : sig
  type t =
    { globPattern : string
    ; kind : int option
    }

  include Json.Jsonable.S with type t := t

  val create : globPattern:string -> ?kind:int -> unit -> t
end

module DidChangeWatchedFilesRegistrationOptions : sig
  type t = { watchers : FileSystemWatcher.t list }

  include Json.Jsonable.S with type t := t

  val create : watchers:FileSystemWatcher.t list -> t
end

module WorkspaceFolder : sig
  type t =
    { uri : DocumentUri.t
    ; name : string
    }

  include Json.Jsonable.S with type t := t

  val create : uri:DocumentUri.t -> name:string -> t
end

module WorkspaceFoldersChangeEvent : sig
  type t =
    { added : WorkspaceFolder.t list
    ; removed : WorkspaceFolder.t list
    }

  include Json.Jsonable.S with type t := t

  val create :
    added:WorkspaceFolder.t list -> removed:WorkspaceFolder.t list -> t
end

module DidChangeWorkspaceFoldersParams : sig
  type t = { event : WorkspaceFoldersChangeEvent.t }

  include Json.Jsonable.S with type t := t

  val create : event:WorkspaceFoldersChangeEvent.t -> t
end

module DidCloseTextDocumentParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> t
end

module TextDocumentItem : sig
  type t =
    { uri : DocumentUri.t
    ; languageId : string
    ; version : int
    ; text : string
    }

  include Json.Jsonable.S with type t := t

  val create :
    uri:DocumentUri.t -> languageId:string -> version:int -> text:string -> t
end

module DidOpenTextDocumentParams : sig
  type t = { textDocument : TextDocumentItem.t }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentItem.t -> t
end

module DidSaveTextDocumentParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; text : string option
    }

  include Json.Jsonable.S with type t := t

  val create :
    textDocument:TextDocumentIdentifier.t -> ?text:string -> unit -> t
end

module DocumentColorOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module DocumentColorParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> t
end

module DocumentColorRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; id : string option
    ; workDoneProgress : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> ?workDoneProgress:bool
    -> unit
    -> t
end

module DocumentFormattingOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module FormattingOptions : sig
  type t =
    { tabSize : int
    ; insertSpaces : bool
    ; trimTrailingWhitespace : bool option
    ; insertFinalNewline : bool option
    ; trimFinalNewlines : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       tabSize:int
    -> insertSpaces:bool
    -> ?trimTrailingWhitespace:bool
    -> ?insertFinalNewline:bool
    -> ?trimFinalNewlines:bool
    -> unit
    -> t
end

module DocumentFormattingParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; options : FormattingOptions.t
    }

  include Json.Jsonable.S with type t := t

  val create :
    textDocument:TextDocumentIdentifier.t -> options:FormattingOptions.t -> t
end

module DocumentFormattingRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
    ?documentSelector:DocumentSelector.t -> ?workDoneProgress:bool -> unit -> t
end

module DocumentHighlightKind : sig
  type t =
    | Text
    | Read
    | Write

  include Json.Jsonable.S with type t := t
end

module DocumentHighlight : sig
  type t =
    { range : Range.t
    ; kind : DocumentHighlightKind.t option
    }

  include Json.Jsonable.S with type t := t

  val create : range:Range.t -> ?kind:DocumentHighlightKind.t -> unit -> t
end

module DocumentHighlightOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module DocumentHighlightParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> position:Position.t -> t
end

module DocumentHighlightRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
    ?documentSelector:DocumentSelector.t -> ?workDoneProgress:bool -> unit -> t
end

module DocumentLink : sig
  type t =
    { range : Range.t
    ; target : DocumentUri.t option
    ; tooltip : string option
    ; data : Json.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       range:Range.t
    -> ?target:DocumentUri.t
    -> ?tooltip:string
    -> ?data:Json.t
    -> unit
    -> t
end

module DocumentLinkOptions : sig
  type t =
    { workDoneProgress : bool option
    ; resolveProvider : bool option
    }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> ?resolveProvider:bool -> unit -> t
end

module DocumentLinkParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> t
end

module DocumentLinkRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; resolveProvider : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?documentSelector:DocumentSelector.t
    -> ?workDoneProgress:bool
    -> ?resolveProvider:bool
    -> unit
    -> t
end

module DocumentOnTypeFormattingOptions : sig
  type t =
    { firstTriggerCharacter : string
    ; moreTriggerCharacter : string list option
    }

  include Json.Jsonable.S with type t := t

  val create :
       firstTriggerCharacter:string
    -> ?moreTriggerCharacter:string list
    -> unit
    -> t
end

module DocumentOnTypeFormattingParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; ch : string
    ; options : FormattingOptions.t
    }

  include Json.Jsonable.S with type t := t

  val create :
       textDocument:TextDocumentIdentifier.t
    -> position:Position.t
    -> ch:string
    -> options:FormattingOptions.t
    -> t
end

module DocumentOnTypeFormattingRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; firstTriggerCharacter : string
    ; moreTriggerCharacter : string list option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?documentSelector:DocumentSelector.t
    -> firstTriggerCharacter:string
    -> ?moreTriggerCharacter:string list
    -> unit
    -> t
end

module DocumentRangeFormattingOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module DocumentRangeFormattingParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; options : FormattingOptions.t
    }

  include Json.Jsonable.S with type t := t

  val create :
       textDocument:TextDocumentIdentifier.t
    -> range:Range.t
    -> options:FormattingOptions.t
    -> t
end

module DocumentRangeFormattingRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
    ?documentSelector:DocumentSelector.t -> ?workDoneProgress:bool -> unit -> t
end

module DocumentSymbol : sig
  type t =
    { name : string
    ; detail : string option
    ; kind : SymbolKind.t
    ; deprecated : bool option
    ; range : Range.t
    ; selectionRange : Range.t
    ; children : t list option
    }

  include Json.Jsonable.S with type t := t

  val create :
       name:string
    -> ?detail:string
    -> kind:SymbolKind.t
    -> ?deprecated:bool
    -> range:Range.t
    -> selectionRange:Range.t
    -> ?children:t list
    -> unit
    -> t
end

module DocumentSymbolOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module DocumentSymbolParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> t
end

module DocumentSymbolRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
    ?documentSelector:DocumentSelector.t -> ?workDoneProgress:bool -> unit -> t
end

module ErrorCodes : sig
  type t =
    | ParseError
    | InvalidRequest
    | MethodNotFound
    | InvalidParams
    | InternalError
    | ServerErrorStart
    | ServerErrorEnd
    | ServerNotInitialized
    | UnknownErrorCode
    | RequestCancelled
    | ContentModified

  include Json.Jsonable.S with type t := t
end

module ExecuteCommandOptions : sig
  type t =
    { workDoneProgress : bool option
    ; commands : string list
    }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> commands:string list -> unit -> t
end

module ExecuteCommandParams : sig
  type t =
    { command : string
    ; arguments : Json.t list option
    }

  include Json.Jsonable.S with type t := t

  val create : command:string -> ?arguments:Json.t list -> unit -> t
end

module ExecuteCommandRegistrationOptions : sig
  type t =
    { workDoneProgress : bool option
    ; commands : string list
    }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> commands:string list -> unit -> t
end

module FileChangeType : sig
  type t =
    | Created
    | Changed
    | Deleted

  include Json.Jsonable.S with type t := t
end

module FoldingRangeKind : sig
  type t =
    | Comment
    | Imports
    | Region

  include Json.Jsonable.S with type t := t
end

module FoldingRange : sig
  type t =
    { startLine : int
    ; startCharacter : int option
    ; endLine : int
    ; endCharacter : int option
    ; kind : FoldingRangeKind.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       startLine:int
    -> ?startCharacter:int
    -> endLine:int
    -> ?endCharacter:int
    -> ?kind:FoldingRangeKind.t
    -> unit
    -> t
end

module FoldingRangeOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module FoldingRangeParams : sig
  type t = { textDocument : TextDocumentIdentifier.t }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> t
end

module FoldingRangeRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; id : string option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?documentSelector:DocumentSelector.t
    -> ?workDoneProgress:bool
    -> ?id:string
    -> unit
    -> t
end

module Hover : sig
  type t =
    { contents :
        [ `MarkedString of MarkedString.t
        | `List of MarkedString.t list
        | `MarkupContent of MarkupContent.t
        ]
    ; range : Range.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       contents:
         [ `MarkedString of MarkedString.t
         | `List of MarkedString.t list
         | `MarkupContent of MarkupContent.t
         ]
    -> ?range:Range.t
    -> unit
    -> t
end

module HoverOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module HoverParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> position:Position.t -> t
end

module HoverRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
    ?documentSelector:DocumentSelector.t -> ?workDoneProgress:bool -> unit -> t
end

module ImplementationOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module ImplementationParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> position:Position.t -> t
end

module ImplementationRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; id : string option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?documentSelector:DocumentSelector.t
    -> ?workDoneProgress:bool
    -> ?id:string
    -> unit
    -> t
end

module InitializeError : sig
  type t = UnknownProtocolVersion

  include Json.Jsonable.S with type t := t
end

module InitializeParams : sig
  type clientInfo =
    { name : string
    ; version : string option
    }

  include Json.Jsonable.S with type t := clientInfo

  val create_clientInfo : name:string -> ?version:string -> unit -> clientInfo

  type t =
    { processId : int option
    ; clientInfo : clientInfo option
    ; rootPath : string option option
    ; rootUri : DocumentUri.t option
    ; initializationOptions : Json.t option
    ; capabilities : ClientCapabilities.t
    ; trace : [ `Off | `Messages | `Verbose ] option
    ; workspaceFolders : WorkspaceFolder.t list option option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?processId:int
    -> ?clientInfo:clientInfo
    -> ?rootPath:string option
    -> ?rootUri:DocumentUri.t
    -> ?initializationOptions:Json.t
    -> capabilities:ClientCapabilities.t
    -> ?trace:[ `Off | `Messages | `Verbose ]
    -> ?workspaceFolders:WorkspaceFolder.t list option
    -> unit
    -> t
end

module WorkspaceFoldersServerCapabilities : sig
  type t =
    { supported : bool option
    ; changeNotifications : [ `String of string | `Bool of bool ] option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?supported:bool
    -> ?changeNotifications:[ `String of string | `Bool of bool ]
    -> unit
    -> t
end

module SelectionRangeOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module SelectionRangeRegistrationOptions : sig
  type t =
    { workDoneProgress : bool option
    ; documentSelector : DocumentSelector.t option
    ; id : string option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?workDoneProgress:bool
    -> ?documentSelector:DocumentSelector.t
    -> ?id:string
    -> unit
    -> t
end

module RenameOptions : sig
  type t =
    { workDoneProgress : bool option
    ; prepareProvider : bool option
    }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> ?prepareProvider:bool -> unit -> t
end

module ReferenceOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module TypeDefinitionOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module TypeDefinitionRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; id : string option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?documentSelector:DocumentSelector.t
    -> ?workDoneProgress:bool
    -> ?id:string
    -> unit
    -> t
end

module SignatureHelpOptions : sig
  type t =
    { workDoneProgress : bool option
    ; triggerCharacters : string list option
    ; retriggerCharacters : string list option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?workDoneProgress:bool
    -> ?triggerCharacters:string list
    -> ?retriggerCharacters:string list
    -> unit
    -> t
end

module SaveOptions : sig
  type t = { includeText : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?includeText:bool -> unit -> t
end

module TextDocumentSyncKind : sig
  type t =
    | None
    | Full
    | Incremental

  include Json.Jsonable.S with type t := t
end

module TextDocumentSyncOptions : sig
  type t =
    { openClose : bool option
    ; change : TextDocumentSyncKind.t option
    ; willSave : bool option
    ; willSaveWaitUntil : bool option
    ; save : SaveOptions.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?openClose:bool
    -> ?change:TextDocumentSyncKind.t
    -> ?willSave:bool
    -> ?willSaveWaitUntil:bool
    -> ?save:SaveOptions.t
    -> unit
    -> t
end

module ServerCapabilities : sig
  type workspace =
    { workspaceFolders : WorkspaceFoldersServerCapabilities.t option }

  include Json.Jsonable.S with type t := workspace

  val create_workspace :
    ?workspaceFolders:WorkspaceFoldersServerCapabilities.t -> unit -> workspace

  type t =
    { textDocumentSync :
        [ `TextDocumentSyncOptions of TextDocumentSyncOptions.t | `Int of int ]
        option
    ; completionProvider : CompletionOptions.t option
    ; hoverProvider : [ `Bool of bool | `HoverOptions of HoverOptions.t ] option
    ; signatureHelpProvider : SignatureHelpOptions.t option
    ; declarationProvider :
        [ `Bool of bool
        | `DeclarationOptions of DeclarationOptions.t
        | `DeclarationRegistrationOptions of DeclarationRegistrationOptions.t
        ]
        option
    ; definitionProvider :
        [ `Bool of bool | `DefinitionOptions of DefinitionOptions.t ] option
    ; typeDefinitionProvider :
        [ `Bool of bool
        | `TypeDefinitionOptions of TypeDefinitionOptions.t
        | `TypeDefinitionRegistrationOptions of
          TypeDefinitionRegistrationOptions.t
        ]
        option
    ; implementationProvider :
        [ `Bool of bool
        | `ImplementationOptions of ImplementationOptions.t
        | `ImplementationRegistrationOptions of
          ImplementationRegistrationOptions.t
        ]
        option
    ; referencesProvider :
        [ `Bool of bool | `ReferenceOptions of ReferenceOptions.t ] option
    ; documentHighlightProvider :
        [ `Bool of bool
        | `DocumentHighlightOptions of DocumentHighlightOptions.t
        ]
        option
    ; documentSymbolProvider :
        [ `Bool of bool | `DocumentSymbolOptions of DocumentSymbolOptions.t ]
        option
    ; codeActionProvider :
        [ `Bool of bool | `CodeActionOptions of CodeActionOptions.t ] option
    ; codeLensProvider : CodeLensOptions.t option
    ; documentLinkProvider : DocumentLinkOptions.t option
    ; colorProvider :
        [ `Bool of bool
        | `DocumentColorOptions of DocumentColorOptions.t
        | `DocumentColorRegistrationOptions of
          DocumentColorRegistrationOptions.t
        ]
        option
    ; documentFormattingProvider :
        [ `Bool of bool
        | `DocumentFormattingOptions of DocumentFormattingOptions.t
        ]
        option
    ; documentRangeFormattingProvider :
        [ `Bool of bool
        | `DocumentRangeFormattingOptions of DocumentRangeFormattingOptions.t
        ]
        option
    ; documentOnTypeFormattingProvider :
        DocumentOnTypeFormattingOptions.t option
    ; renameProvider :
        [ `Bool of bool | `RenameOptions of RenameOptions.t ] option
    ; foldingRangeProvider :
        [ `Bool of bool
        | `FoldingRangeOptions of FoldingRangeOptions.t
        | `FoldingRangeRegistrationOptions of FoldingRangeRegistrationOptions.t
        ]
        option
    ; executeCommandProvider : ExecuteCommandOptions.t option
    ; selectionRangeProvider :
        [ `Bool of bool
        | `SelectionRangeOptions of SelectionRangeOptions.t
        | `SelectionRangeRegistrationOptions of
          SelectionRangeRegistrationOptions.t
        ]
        option
    ; workspaceSymbolProvider : bool option
    ; workspace : workspace option
    ; experimental : Json.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?textDocumentSync:
         [ `TextDocumentSyncOptions of TextDocumentSyncOptions.t | `Int of int ]
    -> ?completionProvider:CompletionOptions.t
    -> ?hoverProvider:[ `Bool of bool | `HoverOptions of HoverOptions.t ]
    -> ?signatureHelpProvider:SignatureHelpOptions.t
    -> ?declarationProvider:
         [ `Bool of bool
         | `DeclarationOptions of DeclarationOptions.t
         | `DeclarationRegistrationOptions of DeclarationRegistrationOptions.t
         ]
    -> ?definitionProvider:
         [ `Bool of bool | `DefinitionOptions of DefinitionOptions.t ]
    -> ?typeDefinitionProvider:
         [ `Bool of bool
         | `TypeDefinitionOptions of TypeDefinitionOptions.t
         | `TypeDefinitionRegistrationOptions of
           TypeDefinitionRegistrationOptions.t
         ]
    -> ?implementationProvider:
         [ `Bool of bool
         | `ImplementationOptions of ImplementationOptions.t
         | `ImplementationRegistrationOptions of
           ImplementationRegistrationOptions.t
         ]
    -> ?referencesProvider:
         [ `Bool of bool | `ReferenceOptions of ReferenceOptions.t ]
    -> ?documentHighlightProvider:
         [ `Bool of bool
         | `DocumentHighlightOptions of DocumentHighlightOptions.t
         ]
    -> ?documentSymbolProvider:
         [ `Bool of bool | `DocumentSymbolOptions of DocumentSymbolOptions.t ]
    -> ?codeActionProvider:
         [ `Bool of bool | `CodeActionOptions of CodeActionOptions.t ]
    -> ?codeLensProvider:CodeLensOptions.t
    -> ?documentLinkProvider:DocumentLinkOptions.t
    -> ?colorProvider:
         [ `Bool of bool
         | `DocumentColorOptions of DocumentColorOptions.t
         | `DocumentColorRegistrationOptions of
           DocumentColorRegistrationOptions.t
         ]
    -> ?documentFormattingProvider:
         [ `Bool of bool
         | `DocumentFormattingOptions of DocumentFormattingOptions.t
         ]
    -> ?documentRangeFormattingProvider:
         [ `Bool of bool
         | `DocumentRangeFormattingOptions of DocumentRangeFormattingOptions.t
         ]
    -> ?documentOnTypeFormattingProvider:DocumentOnTypeFormattingOptions.t
    -> ?renameProvider:[ `Bool of bool | `RenameOptions of RenameOptions.t ]
    -> ?foldingRangeProvider:
         [ `Bool of bool
         | `FoldingRangeOptions of FoldingRangeOptions.t
         | `FoldingRangeRegistrationOptions of FoldingRangeRegistrationOptions.t
         ]
    -> ?executeCommandProvider:ExecuteCommandOptions.t
    -> ?selectionRangeProvider:
         [ `Bool of bool
         | `SelectionRangeOptions of SelectionRangeOptions.t
         | `SelectionRangeRegistrationOptions of
           SelectionRangeRegistrationOptions.t
         ]
    -> ?workspaceSymbolProvider:bool
    -> ?workspace:workspace
    -> ?experimental:Json.t
    -> unit
    -> t
end

module InitializeResult : sig
  type serverInfo =
    { name : string
    ; version : string option
    }

  include Json.Jsonable.S with type t := serverInfo

  val create_serverInfo : name:string -> ?version:string -> unit -> serverInfo

  type t =
    { capabilities : ServerCapabilities.t
    ; serverInfo : serverInfo option
    }

  include Json.Jsonable.S with type t := t

  val create :
    capabilities:ServerCapabilities.t -> ?serverInfo:serverInfo -> unit -> t
end

module LocationLink : sig
  type t =
    { originSelectionRange : Range.t option
    ; targetUri : DocumentUri.t
    ; targetRange : Range.t
    ; targetSelectionRange : Range.t
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?originSelectionRange:Range.t
    -> targetUri:DocumentUri.t
    -> targetRange:Range.t
    -> targetSelectionRange:Range.t
    -> unit
    -> t
end

module LogMessageParams : sig
  type t =
    { type_ : int
    ; message : string
    }

  include Json.Jsonable.S with type t := t

  val create : type_:int -> message:string -> t
end

module MessageActionItem : sig
  type t = { title : string }

  include Json.Jsonable.S with type t := t

  val create : title:string -> t
end

module MessageType : sig
  type t =
    | Error
    | Warning
    | Info
    | Log

  include Json.Jsonable.S with type t := t
end

module ParameterInformation : sig
  type t =
    { label : [ `String of string | `Offset of int * int ]
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ] option
    }

  include Json.Jsonable.S with type t := t

  val create :
       label:[ `String of string | `Offset of int * int ]
    -> ?documentation:[ `String of string | `MarkupContent of MarkupContent.t ]
    -> unit
    -> t
end

module PrepareRenameParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> position:Position.t -> t
end

module ProgressParams : sig
  type t =
    { token : ProgressToken.t
    ; value : Json.t
    }

  include Json.Jsonable.S with type t := t

  val create : token:ProgressToken.t -> value:Json.t -> t
end

module PublishDiagnosticsParams : sig
  type t =
    { uri : DocumentUri.t
    ; version : int option
    ; diagnostics : Diagnostic.t list
    }

  include Json.Jsonable.S with type t := t

  val create :
       uri:DocumentUri.t
    -> ?version:int
    -> diagnostics:Diagnostic.t list
    -> unit
    -> t
end

module ReferenceContext : sig
  type t = { includeDeclaration : bool }

  include Json.Jsonable.S with type t := t

  val create : includeDeclaration:bool -> t
end

module ReferenceParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; context : ReferenceContext.t
    }

  include Json.Jsonable.S with type t := t

  val create :
       textDocument:TextDocumentIdentifier.t
    -> position:Position.t
    -> context:ReferenceContext.t
    -> t
end

module ReferenceRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
    ?documentSelector:DocumentSelector.t -> ?workDoneProgress:bool -> unit -> t
end

module Registration : sig
  type t =
    { id : string
    ; method_ : string
    ; registerOptions : Json.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
    id:string -> method_:string -> ?registerOptions:Json.t -> unit -> t
end

module RegistrationParams : sig
  type t = { registrations : Registration.t list }

  include Json.Jsonable.S with type t := t

  val create : registrations:Registration.t list -> t
end

module RenameParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; newName : string
    }

  include Json.Jsonable.S with type t := t

  val create :
       textDocument:TextDocumentIdentifier.t
    -> position:Position.t
    -> newName:string
    -> t
end

module RenameRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; prepareProvider : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?documentSelector:DocumentSelector.t
    -> ?workDoneProgress:bool
    -> ?prepareProvider:bool
    -> unit
    -> t
end

module SelectionRange : sig
  type t =
    { range : Range.t
    ; parent : t option
    }

  include Json.Jsonable.S with type t := t

  val create : range:Range.t -> ?parent:t -> unit -> t
end

module SelectionRangeParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; positions : Position.t list
    }

  include Json.Jsonable.S with type t := t

  val create :
    textDocument:TextDocumentIdentifier.t -> positions:Position.t list -> t
end

module ShowMessageParams : sig
  type t =
    { type_ : MessageType.t
    ; message : string
    }

  include Json.Jsonable.S with type t := t

  val create : type_:MessageType.t -> message:string -> t
end

module ShowMessageRequestParams : sig
  type t =
    { type_ : int
    ; message : string
    ; actions : MessageActionItem.t list option
    }

  include Json.Jsonable.S with type t := t

  val create :
       type_:int
    -> message:string
    -> ?actions:MessageActionItem.t list
    -> unit
    -> t
end

module SignatureInformation : sig
  type t =
    { label : string
    ; documentation :
        [ `String of string | `MarkupContent of MarkupContent.t ] option
    ; parameters : ParameterInformation.t list option
    }

  include Json.Jsonable.S with type t := t

  val create :
       label:string
    -> ?documentation:[ `String of string | `MarkupContent of MarkupContent.t ]
    -> ?parameters:ParameterInformation.t list
    -> unit
    -> t
end

module SignatureHelp : sig
  type t =
    { signatures : SignatureInformation.t list
    ; activeSignature : int option
    ; activeParameter : int option
    }

  include Json.Jsonable.S with type t := t

  val create :
       signatures:SignatureInformation.t list
    -> ?activeSignature:int
    -> ?activeParameter:int
    -> unit
    -> t
end

module SignatureHelpTriggerKind : sig
  type t =
    | Invoked
    | TriggerCharacter
    | ContentChange

  include Json.Jsonable.S with type t := t
end

module SignatureHelpContext : sig
  type t =
    { triggerKind : SignatureHelpTriggerKind.t
    ; triggerCharacter : string option
    ; isRetrigger : bool
    ; activeSignatureHelp : SignatureHelp.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       triggerKind:SignatureHelpTriggerKind.t
    -> ?triggerCharacter:string
    -> isRetrigger:bool
    -> ?activeSignatureHelp:SignatureHelp.t
    -> unit
    -> t
end

module SignatureHelpParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    ; context : SignatureHelpContext.t option
    }

  include Json.Jsonable.S with type t := t

  val create :
       textDocument:TextDocumentIdentifier.t
    -> position:Position.t
    -> ?context:SignatureHelpContext.t
    -> unit
    -> t
end

module SignatureHelpRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; workDoneProgress : bool option
    ; triggerCharacters : string list option
    ; retriggerCharacters : string list option
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?documentSelector:DocumentSelector.t
    -> ?workDoneProgress:bool
    -> ?triggerCharacters:string list
    -> ?retriggerCharacters:string list
    -> unit
    -> t
end

module SymbolInformation : sig
  type t =
    { name : string
    ; kind : SymbolKind.t
    ; deprecated : bool option
    ; location : Location.t
    ; containerName : string option
    }

  include Json.Jsonable.S with type t := t

  val create :
       name:string
    -> kind:SymbolKind.t
    -> ?deprecated:bool
    -> location:Location.t
    -> ?containerName:string
    -> unit
    -> t
end

module TextDocumentChangeRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; syncKind : TextDocumentSyncKind.t
    }

  include Json.Jsonable.S with type t := t

  val create :
       ?documentSelector:DocumentSelector.t
    -> syncKind:TextDocumentSyncKind.t
    -> unit
    -> t
end

module TextDocumentSaveReason : sig
  type t =
    | Manual
    | AfterDelay
    | FocusOut

  include Json.Jsonable.S with type t := t
end

module TextDocumentSaveRegistrationOptions : sig
  type t =
    { documentSelector : DocumentSelector.t option
    ; includeText : bool option
    }

  include Json.Jsonable.S with type t := t

  val create :
    ?documentSelector:DocumentSelector.t -> ?includeText:bool -> unit -> t
end

module TypeDefinitionParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; position : Position.t
    }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> position:Position.t -> t
end

module Unregistration : sig
  type t =
    { id : string
    ; method_ : string
    }

  include Json.Jsonable.S with type t := t

  val create : id:string -> method_:string -> t
end

module UnregistrationParams : sig
  type t = { unregisterations : Unregistration.t list }

  include Json.Jsonable.S with type t := t

  val create : unregisterations:Unregistration.t list -> t
end

module WatchKind : sig
  type t =
    | Create
    | Change
    | Delete

  include Json.Jsonable.S with type t := t
end

module WillSaveTextDocumentParams : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; reason : int
    }

  include Json.Jsonable.S with type t := t

  val create : textDocument:TextDocumentIdentifier.t -> reason:int -> t
end

module WorkDoneProgressBegin : sig
  type t =
    { title : string
    ; cancellable : bool option
    ; message : string option
    ; percentage : int option
    }

  include Json.Jsonable.S with type t := t

  val create :
       title:string
    -> ?cancellable:bool
    -> ?message:string
    -> ?percentage:int
    -> unit
    -> t
end

module WorkDoneProgressCancelParams : sig
  type t = { token : ProgressToken.t }

  include Json.Jsonable.S with type t := t

  val create : token:ProgressToken.t -> t
end

module WorkDoneProgressCreateParams : sig
  type t = { token : ProgressToken.t }

  include Json.Jsonable.S with type t := t

  val create : token:ProgressToken.t -> t
end

module WorkDoneProgressEnd : sig
  type t = { message : string option }

  include Json.Jsonable.S with type t := t

  val create : ?message:string -> unit -> t
end

module WorkDoneProgressReport : sig
  type t =
    { cancellable : bool option
    ; message : string option
    ; percentage : int option
    }

  include Json.Jsonable.S with type t := t

  val create :
    ?cancellable:bool -> ?message:string -> ?percentage:int -> unit -> t
end

module WorkspaceSymbolOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

module WorkspaceSymbolParams : sig
  type t = { query : string }

  include Json.Jsonable.S with type t := t

  val create : query:string -> t
end

module WorkspaceSymbolRegistrationOptions : sig
  type t = { workDoneProgress : bool option }

  include Json.Jsonable.S with type t := t

  val create : ?workDoneProgress:bool -> unit -> t
end

(*$*)

module CodeActionResult : sig
  type t = [ `Command of Command.t | `CodeAction of CodeAction.t ] list option

  include Json.Jsonable.S with type t := t
end

module Locations : sig
  type t =
    [ `Location of Location.t list
    | `LocationLink of LocationLink.t list
    ]

  include Json.Jsonable.S with type t := t
end
