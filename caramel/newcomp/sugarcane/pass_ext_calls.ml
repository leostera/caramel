(**

   Rewrite calls to module functions from field-access to module-function form.

*)

open Ir

module Mf_table = struct
  open Sexplib.Std

  let build ir =
    let tbl = Hashtbl.create 1024 in

    let arity expr =
      match expr with Ir_fun (args, _) -> List.length args | _ -> -1
    in

    let load_table id =
      let name = Identifier.to_string id in
      let meta_table = Pass_metadata_table.Mf_table.load_module name in
      List.iter (fun (k, v) -> Hashtbl.add tbl k v) meta_table.entries
    in

    let rec aux path idx ir =
      match ir with
      | Ir_module (mod_name, next) -> aux mod_name 0 next
      | Ir_letrec (bindings, next) ->
          List.iteri
            (fun i (_v, name, body) ->
              if Identifier.to_string path != "" then
                Hashtbl.add tbl
                  (path.unique_name, idx + i)
                  (path, name, arity body);
              aux path (idx + i) body)
            bindings;
          aux path (idx + List.length bindings) next
      | Ir_let (_v, name, body, next) ->
          if Identifier.to_string path != "" then
            Hashtbl.add tbl (path.unique_name, idx) (path, name, arity body);
          aux path idx body;
          aux path (idx + 1) next
      | Ir_apply (Ir_field (_, _, Ir_var id), args) ->
          (* when Identifier.is_module id -> *)
          List.iter (aux path idx) args;
          load_table id
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
      | Ir_field (_idx, _field, Ir_var id) when Identifier.is_module id ->
          load_table id
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
    | Some (m, f, a) -> Some (m, f, a)
    | None -> (
        match Hashtbl.find_opt tbl (Identifier.(m.source_name), idx) with
        | Some (m, f, a) -> Some (m, f, a)
        | None -> None)

  let rec find_by_field tbl field =
    let ( let* ) = Option.bind in

    match field with
    | Ir_field (idx, _field, Ir_var m) when Identifier.is_module m ->
        find tbl m idx
    | Ir_field (idx, _field, (Ir_field (_, _, _) as next)) ->
        let* _, m, _ = find_by_field tbl next in
        let* m2, f, arity = find tbl m idx in
        Some (m2, f, arity)
    | _ -> None
end

let rec replace ~to_funref tbl ir =
  match ir with
  (* NOTE: Handle turning function application into external function calls *)
  | Ir_apply ((Ir_field (_, _, _) as field), args) -> (
      let args = List.map (replace ~to_funref:true tbl) args in
      match Mf_table.find_by_field tbl field with
      | Some (m, f, _arity) ->
          let m = Identifier.to_string m in
          let f = Identifier.to_string f in
          Ir.ext_call ~name:(m, f) ~args
      | None -> ir)
  (* NOTE: Handle turning arguments into external function references *)
  | Ir_field (_, _, Ir_var name) as field when to_funref -> (
      match Mf_table.find_by_field tbl field with
      | Some (_m, _f, arity) -> Ir.fn_name ~name ~arity
      | None -> ir)
  (* recursion boilerplate below *)
  | Ir_apply (e, args) ->
      Ir_apply
        ( replace ~to_funref:false tbl e,
          List.map (replace ~to_funref:true tbl) args )
  | Ir_let (v, id, e1, e2) ->
      Ir_let
        (v, id, replace ~to_funref:false tbl e1, replace ~to_funref:false tbl e2)
  | Ir_case (e, pats) ->
      let e = replace ~to_funref:false tbl e in
      let pats =
        List.map (fun (pat, e2) -> (pat, replace ~to_funref:false tbl e2)) pats
      in
      Ir_case (e, pats)
  | Ir_catch (e1, e2) ->
      Ir_catch (replace ~to_funref:false tbl e1, replace ~to_funref:false tbl e2)
  | Ir_ext_call (mf, args) ->
      let args = List.map (replace ~to_funref:true tbl) args in
      Ir_ext_call (mf, args)
  | Ir_field (idx, f, e) -> Ir_field (idx, f, replace ~to_funref:false tbl e)
  | Ir_fun (args, body) -> Ir_fun (args, replace ~to_funref:false tbl body)
  | Ir_letrec (bindings, e) ->
      let bindings =
        List.map
          (fun (v, id, e2) -> (v, id, replace ~to_funref:false tbl e2))
          bindings
      in
      let e = replace ~to_funref:false tbl e in
      Ir_letrec (bindings, e)
  | Ir_module (id, e) -> Ir_module (id, replace ~to_funref:false tbl e)
  | Ir_record { fields } ->
      let fields =
        List.map (fun (idx, e) -> (idx, replace ~to_funref:false tbl e)) fields
      in
      Ir_record { fields }
  | Ir_throw (idx, exprs) ->
      Ir_throw (idx, List.map (replace ~to_funref:false tbl) exprs)
  | Ir_seq (e1, e2) ->
      Ir_seq (replace ~to_funref:false tbl e1, replace ~to_funref:false tbl e2)
  | Ir_cons (h, t) ->
      Ir_cons (replace ~to_funref:false tbl h, replace ~to_funref:false tbl t)
  | Ir_tuple parts -> Ir_tuple (List.map (replace ~to_funref:false tbl) parts)
  | Ir_program parts ->
      Ir_program (List.map (replace ~to_funref:false tbl) parts)
  | Ir_nil | Ir_fn_name (_, _) | Ir_lit _ | Ir_var _ -> ir

let run ir =
  let tbl = Mf_table.build ir in
  replace ~to_funref:false tbl ir
