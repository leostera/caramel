(**

   Rewrite calls to module functions from field-access to module-function form.

*)

open Ir

module Mf_table = struct
  open Sexplib.Std

  type entry = (string * int) * (Identifier.t * Identifier.t) [@@deriving sexp]

  type table = { entries : entry list } [@@deriving sexp]

  (** We will build a table of tables of bindings like:

    {
      ModName:  {
        (UniqueName, Idx): (FullPath, FullName)
      }
    }

    That we can turn into a series of files: [ModName.cml] that include all
    the bindings available.

    This is so that other modules have access to this information.

  *)
  let build ir =
    let meta_tbl = Hashtbl.create 1024 in

    let rec aux tbl path idx ir =
      match ir with
      | Ir_letrec (bindings, next) ->
          List.iteri
            (fun i (_v, name, body) ->
              if Identifier.to_string path != "" then
                Hashtbl.add tbl (path.unique_name, idx + i) (path, name);
              aux tbl path (idx + i) body)
            bindings;
          aux tbl path (idx + List.length bindings) next
      | Ir_let (_v, name, body, next) ->
          if Identifier.to_string path != "" then
            Hashtbl.add tbl (path.unique_name, idx) (path, name);
          aux tbl path idx body;
          aux tbl path (idx + 1) next
      | Ir_module (mod_name, next) ->
          let tbl = Hashtbl.create 1024 in
          Hashtbl.add meta_tbl mod_name tbl;
          aux tbl mod_name 0 next
      | Ir_program parts -> List.iter (aux tbl Identifier.empty 0) parts
      | _ -> ()
    in
    let dummy_table = Hashtbl.create 0 in
    aux dummy_table Identifier.empty 0 ir;
    meta_tbl

  let save tbl =
    let tables = Hashtbl.to_seq tbl |> List.of_seq in

    List.iter
      (fun (mod_name, table) ->
        let filename = Identifier.to_string mod_name ^ ".cml.meta" in
        let entries = Hashtbl.to_seq table |> List.of_seq in
        let table = sexp_of_table { entries } in
        let oc = open_out_bin filename in
        (try
           let ppf = Format.formatter_of_out_channel oc in
           Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) table
         with _ -> Sys.remove filename);
        close_out oc)
      tables

  let load_module mod_name =
    match Bos.OS.File.read (Fpath.v (mod_name ^ ".cml.meta")) with
    | Ok content ->
        let sexp = Sexplib.Sexp.of_string content in
        table_of_sexp sexp
    | _ -> Error.panic ("Could not find .cml.meta for " ^ mod_name)
end

let build_table ir =
  let tbl = Mf_table.build ir in
  Mf_table.save tbl;
  ir
