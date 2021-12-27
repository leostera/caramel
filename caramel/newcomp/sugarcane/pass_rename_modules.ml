(**

   Rewrite module names to be absolute and not rely on nesting.

   For example:

   ```ocaml
   module A = struct
     module B = struct
       (* ... *)
     end
   end
   ```

   Would yield two modules: `A`, and `B`, where OCaml will give them
   a unique name that is good for flattening.

   We want the two modules to be named: `A` and `A.B`, so in later
   stages of code genreation we can spit out the actual full path.

   If we encounter any modules that are clearly not defined within this one, we
   will assume they are _global modules_ and prepend the `Caramel.` prefix to
   their ids.

   To do this, we will keep track of names that have been renamed and are local
   (eg. submodules to the current module, defined in the same file).

*)

open Ir

module Def_table = struct
  let make () = Hashtbl.create 256

  let add ~tbl id ns_id = Hashtbl.add tbl id ns_id

  let is_local ~tbl id = Hashtbl.find_opt tbl id
end

let rec rename_defs ~tbl path ir =
  match ir with
  | Ir_module (id, e) ->
      let ns_id = Identifier.namespace id path in
      Def_table.add ~tbl id ns_id;
      Ir_module (ns_id, rename_defs ~tbl (id :: path) e)
  (* NOTE: the rest below is recursion boilerplate *)
  | Ir_apply (e, args) ->
      Ir_apply (rename_defs ~tbl path e, List.map (rename_defs ~tbl path) args)
  | Ir_let (v, id, e1, e2) ->
      Ir_let (v, id, rename_defs ~tbl path e1, rename_defs ~tbl path e2)
  | Ir_case (e, pats) ->
      let e = rename_defs ~tbl path e in
      let pats =
        List.map (fun (pat, e2) -> (pat, rename_defs ~tbl path e2)) pats
      in
      Ir_case (e, pats)
  | Ir_catch (e1, e2) ->
      Ir_catch (rename_defs ~tbl path e1, rename_defs ~tbl path e2)
  | Ir_ext_call (mf, args) ->
      let args = List.map (rename_defs ~tbl path) args in
      Ir_ext_call (mf, args)
  | Ir_field (idx, field, e) -> Ir_field (idx, field, rename_defs ~tbl path e)
  | Ir_fun (args, body) -> Ir_fun (args, rename_defs ~tbl path body)
  | Ir_letrec (bindings, e) ->
      let bindings =
        List.map (fun (v, id, e2) -> (v, id, rename_defs ~tbl path e2)) bindings
      in
      let e = rename_defs ~tbl path e in
      Ir_letrec (bindings, e)
  | Ir_record { fields } ->
      let fields =
        List.map (fun (idx, e) -> (idx, rename_defs ~tbl path e)) fields
      in
      Ir_record { fields }
  | Ir_throw (idx, exprs) ->
      Ir_throw (idx, List.map (rename_defs ~tbl path) exprs)
  | Ir_seq (e1, e2) ->
      Ir_seq (rename_defs ~tbl path e1, rename_defs ~tbl path e2)
  | Ir_tuple parts -> Ir_tuple (List.map (rename_defs ~tbl path) parts)
  | Ir_program parts -> Ir_program (List.map (rename_defs ~tbl path) parts)
  | Ir_cons (h, t) -> Ir_cons (rename_defs ~tbl path h, rename_defs ~tbl path t)
  | Ir_nil | Ir_fn_name (_, _) | Ir_lit _ | Ir_var _ -> ir

let rec rename_globals ~tbl ir =
  match ir with
  | Ir_field (idx, field, Ir_var ({ source_name; _ } as id))
    when Identifier.is_module id ->
      let id =
        match Def_table.is_local ~tbl id with
        | Some ns_id -> ns_id
        | None -> { id with source_name = "Caramel." ^ source_name }
      in
      Ir_field (idx, field, Ir_var id)
  (* NOTE: the rest below is recursion boilerplate *)
  | Ir_module (id, e) -> Ir_module (id, rename_globals ~tbl e)
  | Ir_apply (e, args) ->
      Ir_apply (rename_globals ~tbl e, List.map (rename_globals ~tbl) args)
  | Ir_let (v, id, e1, e2) ->
      Ir_let (v, id, rename_globals ~tbl e1, rename_globals ~tbl e2)
  | Ir_case (e, pats) ->
      let e = rename_globals ~tbl e in
      let pats =
        List.map (fun (pat, e2) -> (pat, rename_globals ~tbl e2)) pats
      in
      Ir_case (e, pats)
  | Ir_catch (e1, e2) ->
      Ir_catch (rename_globals ~tbl e1, rename_globals ~tbl e2)
  | Ir_ext_call (mf, args) ->
      let args = List.map (rename_globals ~tbl) args in
      Ir_ext_call (mf, args)
  | Ir_field (idx, field, e) -> Ir_field (idx, field, rename_globals ~tbl e)
  | Ir_fun (args, body) -> Ir_fun (args, rename_globals ~tbl body)
  | Ir_letrec (bindings, e) ->
      let bindings =
        List.map (fun (v, id, e2) -> (v, id, rename_globals ~tbl e2)) bindings
      in
      let e = rename_globals ~tbl e in
      Ir_letrec (bindings, e)
  | Ir_record { fields } ->
      let fields =
        List.map (fun (idx, e) -> (idx, rename_globals ~tbl e)) fields
      in
      Ir_record { fields }
  | Ir_throw (idx, exprs) -> Ir_throw (idx, List.map (rename_globals ~tbl) exprs)
  | Ir_seq (e1, e2) -> Ir_seq (rename_globals ~tbl e1, rename_globals ~tbl e2)
  | Ir_tuple parts -> Ir_tuple (List.map (rename_globals ~tbl) parts)
  | Ir_program parts -> Ir_program (List.map (rename_globals ~tbl) parts)
  | Ir_cons (h, t) -> Ir_cons (rename_globals ~tbl h, rename_globals ~tbl t)
  | Ir_nil | Ir_fn_name (_, _) | Ir_lit _ | Ir_var _ -> ir

let run ir =
  let tbl = Def_table.make () in
  ir |> rename_defs ~tbl [] |> rename_globals ~tbl
