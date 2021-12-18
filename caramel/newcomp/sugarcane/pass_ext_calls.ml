(**

   Rewrite calls to module functions from field-access to module-function form.

*)

open Ir

module Mf_table = struct
  let build ir =
    let tbl = Hashtbl.create 8 in
    let rec aux path idx ir =
      match ir with
      | Ir_module (mod_name, next) -> aux mod_name 0 next
      | Ir_let (name, body, next) ->
          if Identifier.to_string path != "" then
            Hashtbl.add tbl (path.unique_name, idx) (path, name);
          aux path idx body;
          aux path (idx + 1) next
      | Ir_program parts -> List.iter (aux Identifier.empty 0) parts
      | _ -> ()
    in
    aux Identifier.empty 0 ir;
    tbl

  let find tbl m idx =
    match Hashtbl.find_opt tbl (Identifier.(m.unique_name), idx) with
    | Some (m, f) -> Some (m, f)
    | None -> None

  let rec find_by_field tbl field =
    let ( let* ) = Option.bind in

    match field with
    | Ir_field (idx, Ir_var m) when Identifier.is_module m -> find tbl m idx
    | Ir_field (idx, (Ir_field (_, _) as next)) ->
        let* _, m = find_by_field tbl next in
        let* m2, f = find tbl m idx in
        Some (m2, f)
    | _ -> None
end

let rec replace tbl ir =
  match ir with
  | Ir_apply ((Ir_field (_, _) as field), args) -> (
      let args = List.map (replace tbl) args in
      match Mf_table.find_by_field tbl field with
      | Some (m, f) ->
          let m = Identifier.to_string m in
          let f = Identifier.to_string f in
          Ir.ext_call ~name:(m, f) ~args
      | None -> ir)
  | Ir_apply (e, args) -> Ir_apply (replace tbl e, List.map (replace tbl) args)
  | Ir_let (id, e1, e2) -> Ir_let (id, replace tbl e1, replace tbl e2)
  | Ir_case (e, pats) ->
      let e = replace tbl e in
      let pats = List.map (fun (pat, e2) -> (pat, replace tbl e2)) pats in
      Ir_case (e, pats)
  | Ir_catch (e1, e2) -> Ir_catch (replace tbl e1, replace tbl e2)
  | Ir_ext_call (mf, args) ->
      let args = List.map (replace tbl) args in
      Ir_ext_call (mf, args)
  | Ir_field (idx, e) -> Ir_field (idx, replace tbl e)
  | Ir_fun (args, body) -> Ir_fun (args, replace tbl body)
  | Ir_letrec (bindings, e) ->
      let bindings = List.map (fun (id, e2) -> (id, replace tbl e2)) bindings in
      let e = replace tbl e in
      Ir_letrec (bindings, e)
  | Ir_module (id, e) -> Ir_module (id, replace tbl e)
  | Ir_record fields ->
      let fields = List.map (fun (idx, e) -> (idx, replace tbl e)) fields in
      Ir_record fields
  | Ir_throw (idx, exprs) -> Ir_throw (idx, List.map (replace tbl) exprs)
  | Ir_seq (e1, e2) -> Ir_seq (replace tbl e1, replace tbl e2)
  | Ir_program parts -> Ir_program (List.map (replace tbl) parts)
  | Ir_lit _ | Ir_var _ -> ir

let run ir = replace (Mf_table.build ir) ir
