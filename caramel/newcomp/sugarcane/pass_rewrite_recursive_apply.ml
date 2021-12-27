(**

   Rewrite local module call to go from `Apply (Var x)` to `Apply (Fn_ref)`.
   This makes it easier to just lift them to B-lang as function references.

*)

open Ir

module Fun_table = struct
  let build ir =
    let tbl = Hashtbl.create 256 in
    let rec aux ir =
      match ir with
      | Ir_module (_, next) -> aux next
      | Ir_let (_, name, Ir_fun (args, _), next) ->
          let arity = List.length args in
          Hashtbl.add tbl Identifier.(name.unique_name) arity;
          aux next
      | Ir_letrec (bindings, next) ->
          List.iter
            (function
              | _, name, Ir_fun (args, _) ->
                  let arity = List.length args in
                  Hashtbl.add tbl Identifier.(name.unique_name) arity
              | _ -> ())
            bindings;
          aux next
      | Ir_program parts -> List.iter aux parts
      | _ -> ()
    in
    aux ir;
    tbl

  let exists tbl name = Hashtbl.find_opt tbl Identifier.(name.unique_name)
end

let rec rewrite tbl ir =
  match ir with
  | Ir_apply ((Ir_var name as fn), args) -> (
      let args = List.map (rewrite tbl) args in
      match Fun_table.exists tbl name with
      | Some arity ->
          let fn = Ir.fn_name ~name ~arity in
          Ir.apply ~fn ~args
      | None -> Ir.apply ~fn ~args)
  | Ir_apply (e, args) -> Ir_apply (rewrite tbl e, List.map (rewrite tbl) args)
  | Ir_let (v, id, e1, e2) -> Ir_let (v, id, rewrite tbl e1, rewrite tbl e2)
  | Ir_case (e, pats) ->
      let e = rewrite tbl e in
      let pats = List.map (fun (pat, e2) -> (pat, rewrite tbl e2)) pats in
      Ir_case (e, pats)
  | Ir_catch (e1, e2) -> Ir_catch (rewrite tbl e1, rewrite tbl e2)
  | Ir_ext_call (mf, args) ->
      let args = List.map (rewrite tbl) args in
      Ir_ext_call (mf, args)
  | Ir_field (idx, field, e) -> Ir_field (idx, field, rewrite tbl e)
  | Ir_fun (args, body) -> Ir_fun (args, rewrite tbl body)
  | Ir_letrec (bindings, e) ->
      let bindings =
        List.map (fun (v, id, e2) -> (v, id, rewrite tbl e2)) bindings
      in
      let e = rewrite tbl e in
      Ir_letrec (bindings, e)
  | Ir_module (id, e) -> Ir_module (id, rewrite tbl e)
  | Ir_record { fields } ->
      let fields = List.map (fun (idx, e) -> (idx, rewrite tbl e)) fields in
      Ir_record { fields }
  | Ir_throw (idx, exprs) -> Ir_throw (idx, List.map (rewrite tbl) exprs)
  | Ir_seq (e1, e2) -> Ir_seq (rewrite tbl e1, rewrite tbl e2)
  | Ir_tuple parts -> Ir_tuple (List.map (rewrite tbl) parts)
  | Ir_program parts -> Ir_program (List.map (rewrite tbl) parts)
  | Ir_cons (h, t) -> Ir_cons (rewrite tbl h, rewrite tbl t)
  | Ir_nil | Ir_lit _ | Ir_var _ | Ir_fn_name (_, _) -> ir

let run ir =
  let tbl = Fun_table.build ir in
  rewrite tbl ir
