(**

   Rewrite calls to module functions from field-access to module-function form.

*)

open Ir

module Mod_table = struct
  let empty = Hashtbl.create 256

  let add t name expr = Hashtbl.add t name expr

  let entries t = Hashtbl.to_seq t |> List.of_seq

  let values t = Hashtbl.to_seq_values t |> List.of_seq
end

(** Traverse the IR and drop any modules in it. *)
let rec strip ir =
  let ( let* ) = Option.bind in

  match ir with
  | Ir_module (id, e) ->
      let* e = strip e in
      Some (Ir_module (id, e))
  | Ir_apply (e, args) -> Some (Ir_apply (e, List.filter_map strip args))
  | Ir_let (_v, _id, Ir_module (_, _), e) -> strip e
  | Ir_let (v, id, e1, e2) ->
      let* e1 = strip e1 in
      let* e2 = strip e2 in
      Some (Ir_let (v, id, e1, e2))
  | Ir_case (e, pats) ->
      let* e = strip e in
      let pats =
        List.filter_map
          (fun (pat, e2) ->
            let* e2 = strip e2 in
            Some (pat, e2))
          pats
      in
      Some (Ir_case (e, pats))
  | Ir_catch (e1, e2) ->
      let* e1 = strip e1 in
      let* e2 = strip e2 in
      Some (Ir_catch (e1, e2))
  | Ir_ext_call (mf, args) ->
      let args = List.filter_map strip args in
      Some (Ir_ext_call (mf, args))
  | Ir_field (idx, field, e) ->
      let* e = strip e in
      Some (Ir_field (idx, field, e))
  | Ir_fun (args, body) ->
      let* body = strip body in
      Some (Ir_fun (args, body))
  | Ir_letrec (bindings, e) ->
      let bindings =
        List.filter_map
          (fun (v, id, e2) ->
            let* e2 = strip e2 in
            Some (v, id, e2))
          bindings
      in
      let* e = strip e in
      Some (Ir_letrec (bindings, e))
  | Ir_record { fields } ->
      let fields =
        List.filter_map
          (fun (idx, e) ->
            let* e = strip e in
            Some (idx, e))
          fields
      in
      Some (Ir_record { fields })
  | Ir_throw (idx, exprs) -> Some (Ir_throw (idx, List.filter_map strip exprs))
  | Ir_cons (h, t) ->
      let* h = strip h in
      let* t = strip t in
      Some (Ir_cons (h, t))
  | Ir_seq (e1, e2) ->
      let* e1 = strip e1 in
      let* e2 = strip e2 in
      Some (Ir_seq (e1, e2))
  | Ir_tuple parts ->
      let parts =
        List.filter_map
          (fun part ->
            let* part = strip part in
            Some part)
          parts
      in
      Some (Ir_tuple parts)
  | Ir_nil | Ir_fn_name (_, _) | Ir_program _ | Ir_lit _ | Ir_var _ -> Some ir

(** Traverse the IR and collect all the modules. Before saving them,
    strip them of their submodules
  *)
let rec flatten mods ir =
  match ir with
  | Ir_module (mod_name, next) -> (
      flatten mods next;
      match strip next with
      | None -> ()
      | Some next ->
          let next = Ir_module (mod_name, next) in
          Mod_table.add mods mod_name next)
  | Ir_apply (e, args) ->
      flatten mods e;
      List.iter (flatten mods) args
  | Ir_let (_v, _id, e1, e2) ->
      flatten mods e1;
      flatten mods e2
  | Ir_case (e, pats) ->
      flatten mods e;
      List.iter (fun (_pat, e2) -> flatten mods e2) pats
  | Ir_catch (e1, e2) ->
      flatten mods e1;
      flatten mods e2
  | Ir_ext_call (_mf, args) -> List.iter (flatten mods) args
  | Ir_field (_idx, _field, e) -> flatten mods e
  | Ir_fun (_args, body) -> flatten mods body
  | Ir_letrec (bindings, e) ->
      List.iter (fun (_v, _id, e2) -> flatten mods e2) bindings;
      flatten mods e
  | Ir_record { fields; _ } -> List.iter (fun (_, e) -> flatten mods e) fields
  | Ir_throw (_idx, exprs) -> List.iter (flatten mods) exprs
  | Ir_seq (e1, e2) ->
      flatten mods e1;
      flatten mods e2
  | Ir_tuple ps | Ir_program ps -> List.iter (flatten mods) ps
  | Ir_cons (h, t) ->
      flatten mods h;
      flatten mods t
  | Ir_nil | Ir_fn_name (_, _) | Ir_lit _ | Ir_var _ -> ()

let run ir =
  let tbl = Mod_table.empty in
  flatten tbl ir;
  Ir.program (Mod_table.values tbl)
