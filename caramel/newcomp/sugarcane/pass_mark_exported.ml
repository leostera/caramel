open Ir

(**
   Marks module function declarations as Exported
*)

module Export_table = struct
  type t = (string, Ir.visibility) Hashtbl.t

  let empty : t = Hashtbl.create 256

  let add t name = Hashtbl.add t Identifier.(name.source_name) Exported

  let visibility t name =
    match Hashtbl.find_opt t Identifier.(name.source_name) with
    | Some x -> x
    | None -> Private
end

(** Traverse the OCaml Signature and find all the exported symbols. *)
let find_exports ~tbl Translation_unit.{ program = impl, _; _ } =
  let find_in_signature s =
    s
    |> List.iter (fun sig_item ->
           match sig_item with
           | Types.Sig_value (name, _vd, Exported) ->
               let name = Identifier.of_ident name in
               Export_table.add tbl name
           | _ -> ())
  in

  find_in_signature impl.signature;

  let open Typedtree in
  List.iter
    (fun str_item ->
      (match str_item.str_desc with
      | Tstr_module mb -> [ mb ]
      | Tstr_recmodule mbs -> mbs
      | _ -> [])
      |> List.iter (fun mb ->
             match mb.mb_expr.mod_desc with
             | Tmod_constraint (_, Mty_signature signature, _, _) ->
                 find_in_signature signature
             | Tmod_structure typedtree -> find_in_signature typedtree.str_type
             | _ -> ()))
    impl.structure.str_items

let rec mark_exported ~tbl ir =
  match ir with
  | Ir_apply (e, args) ->
      Ir_apply (mark_exported ~tbl e, List.map (mark_exported ~tbl) args)
  | Ir_let (_, id, e1, e2) ->
      let visibility = Export_table.visibility tbl id in
      Ir_let (visibility, id, mark_exported ~tbl e1, mark_exported ~tbl e2)
  | Ir_case (e, pats) ->
      let e = mark_exported ~tbl e in
      let pats =
        List.map (fun (pat, e2) -> (pat, mark_exported ~tbl e2)) pats
      in
      Ir_case (e, pats)
  | Ir_catch (e1, e2) -> Ir_catch (mark_exported ~tbl e1, mark_exported ~tbl e2)
  | Ir_ext_call (mf, args) ->
      let args = List.map (mark_exported ~tbl) args in
      Ir_ext_call (mf, args)
  | Ir_field (idx, field, e) -> Ir_field (idx, field, mark_exported ~tbl e)
  | Ir_fun (args, body) -> Ir_fun (args, mark_exported ~tbl body)
  | Ir_letrec (bindings, e) ->
      let bindings =
        List.map
          (fun (_v, id, e2) ->
            (Export_table.visibility tbl id, id, mark_exported ~tbl e2))
          bindings
      in
      let e = mark_exported ~tbl e in
      Ir_letrec (bindings, e)
  | Ir_module (id, e) -> Ir_module (id, mark_exported ~tbl e)
  | Ir_record { fields } ->
      let fields =
        List.map (fun (idx, e) -> (idx, mark_exported ~tbl e)) fields
      in
      Ir_record { fields }
  | Ir_throw (idx, exprs) -> Ir_throw (idx, List.map (mark_exported ~tbl) exprs)
  | Ir_seq (e1, e2) -> Ir_seq (mark_exported ~tbl e1, mark_exported ~tbl e2)
  | Ir_tuple parts -> Ir_tuple (List.map (mark_exported ~tbl) parts)
  | Ir_program parts -> Ir_program (List.map (mark_exported ~tbl) parts)
  | Ir_cons (h, t) -> Ir_cons (mark_exported ~tbl h, mark_exported ~tbl t)
  | Ir_nil | Ir_fn_name (_, _) | Ir_lit _ | Ir_var _ -> ir

let run ~tunit ir =
  let tbl = Export_table.empty in
  find_exports ~tbl tunit;
  mark_exported ~tbl ir
