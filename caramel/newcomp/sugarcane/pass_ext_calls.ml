(**

   Rewrite calls to module functions from field-access to module-function form.

*)

open Ir

module Mf_table = struct
  open Sexplib.Std

  let build ir =
    let tbl = Hashtbl.create 1024 in
    let rec aux path idx ir =
      match ir with
      | Ir_module (mod_name, next) -> aux mod_name 0 next
      | Ir_letrec (bindings, next) ->
          List.iteri
            (fun i (_v, name, body) ->
              if Identifier.to_string path != "" then
                Hashtbl.add tbl (path.unique_name, idx + i) (path, name);
              aux path (idx + i) body)
            bindings;
          aux path (idx + List.length bindings) next
      | Ir_let (_v, name, body, next) ->
          if Identifier.to_string path != "" then
            Hashtbl.add tbl (path.unique_name, idx) (path, name);
          aux path idx body;
          aux path (idx + 1) next
      | Ir_apply (Ir_field (_, _, Ir_var id), args) ->
          (* when Identifier.is_module id -> *)
          List.iter (aux path idx) args;
          let meta_table =
            Pass_metadata_table.Mf_table.load_module (Identifier.to_string id)
          in
          List.iter (fun (k, v) -> Hashtbl.add tbl k v) meta_table.entries
      | Ir_program parts -> List.iter (aux Identifier.empty 0) parts
      | Ir_fun (_, body) -> aux path idx body
      | Ir_apply (e, args) ->
          aux path idx e;
          List.iter (aux path idx) args
      | Ir_case (e, pats) ->
          aux path idx e;
          List.iter (fun (_pat, e2) -> aux path idx e2) pats
      | Ir_catch (e1, e2) ->
          aux path idx e1;
          aux path idx e2
      | Ir_ext_call (_mf, args) -> List.iter (aux path idx) args
      | Ir_field (_idx, _field, e) -> aux path idx e
      | Ir_record { fields } ->
          List.iter (fun (_idx, e) -> aux path idx e) fields
      | Ir_throw (idx, exprs) -> List.iter (aux path idx) exprs
      | Ir_seq (e1, e2) ->
          aux path idx e1;
          aux path idx e2
      | Ir_tuple parts -> List.iter (aux path idx) parts
      | Ir_cons (h, t) ->
          aux path idx h;
          aux path idx t
      | Ir_nil | Ir_fn_name (_, _) | Ir_lit _ | Ir_var _ -> ()
    in
    aux Identifier.empty 0 ir;
    tbl

  let find tbl m idx =
    match Hashtbl.find_opt tbl (Identifier.(m.unique_name), idx) with
    | Some (m, f) -> Some (m, f)
    | None -> (
        match Hashtbl.find_opt tbl (Identifier.(m.source_name), idx) with
        | Some (m, f) -> Some (m, f)
        | None -> None)

  let rec find_by_field tbl field =
    let ( let* ) = Option.bind in

    match field with
    | Ir_field (idx, _field, Ir_var m) when Identifier.is_module m ->
        find tbl m idx
    | Ir_field (idx, _field, (Ir_field (_, _, _) as next)) ->
        let* _, m = find_by_field tbl next in
        let* m2, f = find tbl m idx in
        Some (m2, f)
    | _ -> None
end

let rec replace tbl ir =
  match ir with
  | Ir_apply ((Ir_field (_, _, _) as field), args) -> (
      let args = List.map (replace tbl) args in
      match Mf_table.find_by_field tbl field with
      | Some (m, f) ->
          let m = Identifier.to_string m in
          let f = Identifier.to_string f in
          Ir.ext_call ~name:(m, f) ~args
      | None -> ir)
  | Ir_apply (e, args) -> Ir_apply (replace tbl e, List.map (replace tbl) args)
  | Ir_let (v, id, e1, e2) -> Ir_let (v, id, replace tbl e1, replace tbl e2)
  | Ir_case (e, pats) ->
      let e = replace tbl e in
      let pats = List.map (fun (pat, e2) -> (pat, replace tbl e2)) pats in
      Ir_case (e, pats)
  | Ir_catch (e1, e2) -> Ir_catch (replace tbl e1, replace tbl e2)
  | Ir_ext_call (mf, args) ->
      let args = List.map (replace tbl) args in
      Ir_ext_call (mf, args)
  | Ir_field (idx, f, e) -> Ir_field (idx, f, replace tbl e)
  | Ir_fun (args, body) -> Ir_fun (args, replace tbl body)
  | Ir_letrec (bindings, e) ->
      let bindings =
        List.map (fun (v, id, e2) -> (v, id, replace tbl e2)) bindings
      in
      let e = replace tbl e in
      Ir_letrec (bindings, e)
  | Ir_module (id, e) -> Ir_module (id, replace tbl e)
  | Ir_record { fields } ->
      let fields = List.map (fun (idx, e) -> (idx, replace tbl e)) fields in
      Ir_record { fields }
  | Ir_throw (idx, exprs) -> Ir_throw (idx, List.map (replace tbl) exprs)
  | Ir_seq (e1, e2) -> Ir_seq (replace tbl e1, replace tbl e2)
  | Ir_cons (h, t) -> Ir_cons (replace tbl h, replace tbl t)
  | Ir_tuple parts -> Ir_tuple (List.map (replace tbl) parts)
  | Ir_program parts -> Ir_program (List.map (replace tbl) parts)
  | Ir_nil | Ir_fn_name (_, _) | Ir_lit _ | Ir_var _ -> ir

let run ir =
  let tbl = Mf_table.build ir in
  replace tbl ir
