open Import

let infer_intf_for_impl doc =
  match Document.kind doc with
  | Intf ->
    Code_error.raise
      "expected an implementation document, got an interface instead" []
  | Impl ->
    Document.with_pipeline doc (fun pipeline ->
        let typer = Mpipeline.typer_result pipeline in
        let pos = Mpipeline.get_lexing_pos pipeline `Start in
        let env, _ = Mbrowse.leaf_node (Mtyper.node_at typer pos) in
        let sig_ : Types.signature =
          let typedtree = Mtyper.get_typedtree typer in
          match typedtree with
          | `Interface _ -> assert false
          | `Implementation doc -> doc.str_type
        in
        Printtyp.wrap_printing_env env (fun () ->
            Format.asprintf "%a@." Printtyp.signature sig_))

let language_id_of_fname s =
  match Filename.extension s with
  | ".mli"
  | ".eliomi" ->
    "ocaml.interface"
  | ".ml"
  | ".eliom" ->
    "ocaml"
  | ".rei"
  | ".re" ->
    "reason"
  | ".mll" -> "ocaml.ocamllex"
  | ".mly" -> "ocaml.menhir"
  | ext ->
    Code_error.raise "unsupported file extension" [ ("extension", String ext) ]

let force_open_document (state : State.t) uri =
  let open Fiber.O in
  let filename = Uri.to_path uri in
  let text = Io.read_file (Fpath.of_string filename) in
  let delay = Configuration.diagnostics_delay state.configuration in
  let timer = Scheduler.create_timer state.scheduler ~delay in
  let languageId = language_id_of_fname filename in
  let text_document =
    Lsp.Types.TextDocumentItem.create ~uri:(Uri.to_string uri) ~languageId
      ~version:0 ~text
  in
  let params = DidOpenTextDocumentParams.create ~textDocument:text_document in
  let+ doc = Document.make timer state.merlin params in
  Document_store.put state.store doc;
  doc

let infer_intf ~force_open_impl (state : State.t) doc =
  let open Fiber.Result.O in
  match Document.kind doc with
  | Impl -> Code_error.raise "the provided document is not an interface." []
  | Intf ->
    let intf_uri = Document.uri doc in
    let impl_uri = Document.get_impl_intf_counterparts intf_uri |> List.hd in
    let* impl =
      match (Document_store.get_opt state.store impl_uri, force_open_impl) with
      | None, false ->
        Code_error.raise
          "The implementation for this interface has not been open." []
      | None, true -> force_open_document state impl_uri |> Fiber.Result.lift
      | Some impl, _ -> Fiber.Result.return impl
    in
    infer_intf_for_impl impl
