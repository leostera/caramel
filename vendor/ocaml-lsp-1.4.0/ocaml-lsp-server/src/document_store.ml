open! Import

type t = (Uri.t, Document.t) Table.t

let make () = Table.create (module Uri) 50

let put store doc = Table.set store (Document.uri doc) doc

let get_opt store = Table.find store

let get store uri =
  match Table.find store uri with
  | Some doc -> Ok doc
  | None ->
    Error
      (Jsonrpc.Response.Error.make ~code:InvalidRequest
         ~message:(Format.asprintf "no document found with uri: %a" Uri.pp uri)
         ())

let remove_document store uri =
  match Table.find store uri with
  | None -> Fiber.return ()
  | Some doc ->
    let open Fiber.O in
    let+ () = Document.close doc in
    Table.remove store uri

let get_size store = Table.length store

let close t =
  let docs = Table.fold t ~init:[] ~f:(fun doc acc -> doc :: acc) in
  let open Fiber.O in
  let+ () = Fiber.parallel_iter docs ~f:Document.close in
  Table.clear t
