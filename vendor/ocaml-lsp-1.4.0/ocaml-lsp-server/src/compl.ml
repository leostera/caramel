open Import

module Resolve = struct
  type t = CompletionParams.t

  let uri (t : t) = Uri.t_of_yojson (`String t.textDocument.uri)

  let yojson_of_t = CompletionParams.yojson_of_t

  let t_of_yojson = CompletionParams.t_of_yojson

  let of_completion_item (ci : CompletionItem.t) =
    match ci.data with
    | Some json -> t_of_yojson json
    | None ->
      Code_error.raise
        "client did not pass additional data to completion resolve" []
end

let completion_kind kind : CompletionItemKind.t option =
  match kind with
  | `Value -> Some Value
  | `Constructor -> Some Constructor
  | `Variant -> None
  | `Label -> Some Property
  | `Module
  | `Modtype ->
    Some Module
  | `Type -> Some TypeParameter
  | `MethodCall -> Some Method
  | `Keyword -> Some Keyword

(** [prefix_of_position ~short_path source position] computes prefix before
    given [position].

    @param short_path determines whether we want full prefix or cut at ["."],
    e.g. [List.m<cursor>] returns ["m"] when [short_path] is set vs ["List.m"]
    when not.
    @return prefix of [position] in [source] and its length *)
let prefix_of_position ~short_path source position =
  match Msource.text source with
  | "" -> ""
  | text ->
    let from =
      let (`Offset index) = Msource.get_offset source position in
      min (String.length text - 1) (index - 1)
    in
    let pos =
      let ident_or_infix_char = function
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9'
        | '\''
        | '_'
        (* Infix function characters *)
        | '$'
        | '&'
        | '*'
        | '+'
        | '-'
        | '/'
        | '='
        | '>'
        | '@'
        | '^'
        | '!'
        | '?'
        | '%'
        | '<'
        | ':'
        | '~'
        | '#' ->
          true
        | '.' -> not short_path
        | _ -> false
      in
      String.rfindi text ~from ~f:(fun c -> not (ident_or_infix_char c))
    in
    let pos =
      match pos with
      | None -> 0
      | Some pos -> pos + 1
    in
    let len = from - pos + 1 in
    String.sub text ~pos ~len

let suffix_of_position source position =
  match Msource.text source with
  | "" -> ""
  | text ->
    let len = String.length text in
    let from =
      let (`Offset index) = Msource.get_offset source position in
      min index (len - 1)
    in
    let len =
      let ident_char = function
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9'
        | '\''
        | '_' ->
          true
        | _ -> false
      in
      let until =
        String.findi ~from text ~f:(fun c -> not (ident_char c))
        |> Option.value ~default:len
      in
      until - from
    in
    String.sub text ~pos:from ~len

let range_prefix (lsp_position : Position.t) prefix : Range.t =
  let start =
    let len = String.length prefix in
    let character = lsp_position.character - len in
    { lsp_position with character }
  in
  { Range.start; end_ = lsp_position }

let item index full_entry ~compl_info =
  let range, (entry : Query_protocol.Compl.entry) = full_entry in
  let kind = completion_kind entry.kind in
  let textEdit = Some { TextEdit.range; newText = entry.name } in
  CompletionItem.create ~label:entry.name ?kind ~detail:entry.desc
    ~deprecated:
      entry.deprecated
      (* Without this field the client is not forced to respect the order
         provided by merlin. *)
    ~sortText:(Printf.sprintf "%04d" index)
    ~data:compl_info ?textEdit ()

let complete doc lsp_position =
  let position = Position.logical lsp_position in

  let prefix =
    prefix_of_position ~short_path:false (Document.source doc) position
  in
  log ~title:Logger.Title.Debug "completion prefix: |%s|" prefix;

  Document.with_pipeline_exn doc @@ fun pipeline ->
  let completion =
    let complete =
      Query_protocol.Complete_prefix (prefix, position, [], false, true)
    in
    Query_commands.dispatch pipeline complete
  in
  let short_range =
    range_prefix lsp_position
      (prefix_of_position ~short_path:true (Document.source doc) position)
  in
  let items =
    completion.entries |> List.map ~f:(fun entry -> (short_range, entry))
  in
  let items =
    match completion.context with
    | `Unknown -> items
    | `Application { Query_protocol.Compl.labels; argument_type = _ } ->
      items
      @ List.map labels ~f:(fun (name, typ) ->
            ( short_range
            , { Query_protocol.Compl.name
              ; kind = `Label
              ; desc = typ
              ; info = ""
              ; deprecated = false (* TODO this is wrong *)
              } ))
  in
  let textDocument =
    TextDocumentIdentifier.create ~uri:(Document.uri doc |> Lsp.Uri.to_string)
  in
  let compl_info =
    CompletionParams.create ~textDocument ~position:lsp_position ()
    |> CompletionParams.yojson_of_t
  in
  let items = List.mapi ~f:(item ~compl_info) items in
  Ok (`CompletionList { CompletionList.isIncomplete = false; items })

let format_doc ~markdown doc =
  match markdown with
  | false -> `String doc
  | true ->
    `MarkupContent
      (match Doc_to_md.translate doc with
      | Markdown value -> { kind = MarkupKind.Markdown; MarkupContent.value }
      | Raw value -> { kind = MarkupKind.PlainText; MarkupContent.value })

let resolve doc (compl : CompletionItem.t) (resolve : Resolve.t) query_doc
    ~markdown =
  (* Due to merlin's API, we create a version of the given document with the
     applied completion item and pass it to merlin to get the docs for the
     [compl.label] *)
  let position : Position.t = resolve.position in
  let complete =
    let logical_position = Position.logical position in
    let start =
      let prefix =
        prefix_of_position ~short_path:true (Document.source doc)
          logical_position
      in
      { position with character = position.character - String.length prefix }
    in
    let end_ =
      let suffix = suffix_of_position (Document.source doc) logical_position in
      { position with character = position.character + String.length suffix }
    in
    let range = Range.create ~start ~end_ in
    TextDocumentContentChangeEvent.create ~range ~text:compl.label ()
  in
  let open Fiber.O in
  let* doc = Document.update_text doc [ complete ] in
  let+ documentation = query_doc doc @@ Position.logical position in
  let documentation = Option.map ~f:(format_doc ~markdown) documentation in
  let compl = { compl with documentation; data = None } in
  Ok compl
