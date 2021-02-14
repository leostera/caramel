open! Import

module Kind = struct
  type t =
    | Intf
    | Impl

  let of_fname p =
    match Filename.extension p with
    | ".ml"
    | ".eliom"
    | ".re" ->
      Impl
    | ".mli"
    | ".eliomi"
    | ".rei" ->
      Intf
    | ext ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make ~code:InvalidRequest
           ~message:(Printf.sprintf "unsupported file extension")
           ~data:(`Assoc [ ("extension", `String ext) ])
           ())
end

module Syntax = struct
  type t =
    | Ocaml
    | Reason
    | Ocamllex
    | Menhir

  let human_name = function
    | Ocaml -> "OCaml"
    | Reason -> "Reason"
    | Ocamllex -> "OCamllex"
    | Menhir -> "Menhir/ocamlyacc"

  let all =
    [ ("ocaml.interface", Ocaml)
    ; ("ocaml", Ocaml)
    ; ("reason", Reason)
    ; ("ocaml.ocamllex", Ocamllex)
    ; ("ocaml.menhir", Menhir)
    ]

  let of_fname s =
    match Filename.extension s with
    | ".eliomi"
    | ".eliom"
    | ".mli"
    | ".ml" ->
      Ocaml
    | ".rei"
    | ".re" ->
      Reason
    | ".mll" -> Ocamllex
    | ".mly" -> Menhir
    | ext ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make ~code:InvalidRequest
           ~message:(Printf.sprintf "unsupported file extension")
           ~data:(`Assoc [ ("extension", `String ext) ])
           ())

  let of_language_id language_id =
    match List.assoc all language_id with
    | Some id -> id
    | None ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make ~code:InvalidRequest
           ~message:(Printf.sprintf "invalid language ID")
           ~data:(`Assoc [ ("language_id", `String language_id) ])
           ())

  let to_language_id x =
    List.find_map all ~f:(fun (k, v) -> Option.some_if (v = x) k)
    |> Option.value_exn

  let markdown_name = function
    | Ocaml -> "ocaml"
    | Reason -> "reason"
    | s -> to_language_id s
end

type t =
  { tdoc : Text_document.t
  ; pipeline : Mpipeline.t
  ; merlin : Scheduler.thread
  ; timer : Scheduler.timer
  }

let uri doc = Text_document.documentUri doc.tdoc

let kind t = Kind.of_fname (Uri.to_path (uri t))

let syntax t = Syntax.of_language_id (Text_document.languageId t.tdoc)

let timer t = t.timer

let source doc = Mpipeline.raw_source doc.pipeline

let await task =
  let open Fiber.O in
  let () = Server.on_cancel (fun () -> Scheduler.cancel_task task) in
  let+ res = Scheduler.await task in
  match res with
  | Error `Canceled ->
    let e =
      Jsonrpc.Response.Error.make ~code:RequestCancelled ~message:"cancelled" ()
    in
    raise (Jsonrpc.Response.Error.E e)
  | Error (`Exn e) -> Error e
  | Ok s -> Ok s

let with_pipeline (doc : t) f =
  Scheduler.async_exn doc.merlin (fun () ->
      Mpipeline.with_pipeline doc.pipeline (fun () -> f doc.pipeline))
  |> await

let with_pipeline_exn doc f =
  let open Fiber.O in
  let+ res = with_pipeline doc f in
  Result.ok_exn res

let version doc = Text_document.version doc.tdoc

let make_config uri =
  let path = Uri.to_path uri in
  let mconfig = Mconfig.initial in
  let path = Misc.canonicalize_filename path in
  let filename = Filename.basename path in
  let directory = Filename.dirname path in
  let mconfig =
    { mconfig with
      query = { mconfig.query with verbosity = 1; filename; directory }
    }
  in
  Mconfig.get_external_config path mconfig

let make_pipeline thread tdoc =
  let async_make_pipeline =
    Scheduler.async_exn thread (fun () ->
        let text = Text_document.text tdoc in
        let source = Msource.make text in
        let config =
          let uri = Text_document.documentUri tdoc in
          make_config uri
        in
        Mpipeline.make config source)
  in
  await async_make_pipeline |> Fiber.map ~f:Result.ok_exn

let make timer merlin_thread tdoc =
  let tdoc = Text_document.make tdoc in
  (* we can do that b/c all text positions in LSP are line/col *)
  let open Fiber.O in
  let+ pipeline = make_pipeline merlin_thread tdoc in
  { tdoc; pipeline; merlin = merlin_thread; timer }

let update_text ?version doc changes =
  let tdoc =
    List.fold_left changes ~init:doc.tdoc ~f:(fun acc change ->
        Text_document.apply_content_change ?version acc change)
  in
  let open Fiber.O in
  let+ pipeline = make_pipeline doc.merlin tdoc in
  { doc with tdoc; pipeline }

let dispatch (doc : t) command =
  with_pipeline doc (fun pipeline -> Query_commands.dispatch pipeline command)

let dispatch_exn (doc : t) command =
  with_pipeline_exn doc (fun pipeline ->
      Query_commands.dispatch pipeline command)

let close t = Scheduler.cancel_timer t.timer

let get_impl_intf_counterparts uri =
  let uri_s = Uri.to_string uri in
  let fpath =
    match String.split ~on:':' uri_s with
    | [ scheme; path ] ->
      if scheme = "file" then
        Uri.t_of_yojson (`String uri_s) |> Uri.to_path
      else
        path
    | _ ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make ~code:InvalidRequest
           ~message:"provided file URI (param) doesn't follow URI spec" ())
  in
  let fname = Filename.basename fpath in
  let ml, mli, eliom, eliomi, re, rei, mll, mly =
    ("ml", "mli", "eliom", "eliomi", "re", "rei", "mll", "mly")
  in
  let exts_to_switch_to =
    match Syntax.of_fname fname with
    | Ocaml -> (
      match Kind.of_fname fname with
      | Intf -> [ ml; mly; mll; eliom; re ]
      | Impl -> [ mli; mly; mll; eliomi; rei ])
    | Reason -> (
      match Kind.of_fname fname with
      | Intf -> [ re; ml ]
      | Impl -> [ rei; mli ])
    | Ocamllex -> [ mli; rei ]
    | Menhir -> [ mli; rei ]
  in
  let fpath_w_ext ext = Filename.remove_extension fpath ^ "." ^ ext in
  let find_switch exts =
    List.filter_map exts ~f:(fun ext ->
        let file_to_switch_to = fpath_w_ext ext in
        Option.some_if (Sys.file_exists file_to_switch_to) file_to_switch_to)
  in
  let files_to_switch_to =
    match find_switch exts_to_switch_to with
    | [] ->
      let switch_to_ext = List.hd exts_to_switch_to in
      let switch_to_fpath = fpath_w_ext switch_to_ext in
      [ switch_to_fpath ]
    | to_switch_to -> to_switch_to
  in
  List.map ~f:Uri.of_path files_to_switch_to
