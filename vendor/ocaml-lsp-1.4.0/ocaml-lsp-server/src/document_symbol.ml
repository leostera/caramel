open Import

let outline_kind kind : SymbolKind.t =
  match kind with
  | `Value -> Function
  | `Constructor -> Constructor
  | `Label -> Property
  | `Module -> Module
  | `Modtype -> Module
  | `Type -> String
  | `Exn -> Constructor
  | `Class -> Class
  | `Method -> Method

let range item = Range.of_loc item.Query_protocol.location

let rec symbol item =
  let children = List.map item.Query_protocol.children ~f:symbol in
  let range : Range.t = range item in
  let kind = outline_kind item.outline_kind in
  DocumentSymbol.create ~name:item.Query_protocol.outline_name ~kind
    ?detail:item.Query_protocol.outline_type ~deprecated:item.deprecated ~range
    ~selectionRange:range ~children ()

let rec symbol_info ?containerName uri item =
  let location =
    let uri = Uri.to_string uri in
    { Location.uri; range = range item }
  in
  let info =
    let kind = outline_kind item.outline_kind in
    SymbolInformation.create ~name:item.Query_protocol.outline_name ~kind
      ~deprecated:false ~location ?containerName ()
  in
  let children =
    List.concat_map item.children ~f:(symbol_info uri ~containerName:info.name)
  in
  info :: children

let run (client_capabilities : ClientCapabilities.t) doc uri =
  let command = Query_protocol.Outline in
  let open Fiber.O in
  let+ outline = Document.dispatch_exn doc command in
  let symbols =
    let hierarchicalDocumentSymbolSupport =
      let open Option.O in
      Option.value
        (let* textDocument = client_capabilities.textDocument in
         let* ds = textDocument.documentSymbol in
         ds.hierarchicalDocumentSymbolSupport)
        ~default:false
    in
    match hierarchicalDocumentSymbolSupport with
    | true ->
      let symbols = List.map outline ~f:symbol in
      `DocumentSymbol symbols
    | false ->
      let symbols = List.concat_map ~f:(symbol_info uri) outline in
      `SymbolInformation symbols
  in
  symbols
