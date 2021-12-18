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

*)

open Ir

let rec rename path ir =
  match ir with
  | Ir_module (id, e) ->
      let ns_id = Identifier.namespace id path in
      Ir_module (ns_id, rename (id :: path) e)
  (* NOTE: the rest below is recursion boilerplate *)
  | Ir_apply (e, args) -> Ir_apply (rename path e, List.map (rename path) args)
  | Ir_let (id, e1, e2) -> Ir_let (id, rename path e1, rename path e2)
  | Ir_case (e, pats) ->
      let e = rename path e in
      let pats = List.map (fun (pat, e2) -> (pat, rename path e2)) pats in
      Ir_case (e, pats)
  | Ir_catch (e1, e2) -> Ir_catch (rename path e1, rename path e2)
  | Ir_ext_call (mf, args) ->
      let args = List.map (rename path) args in
      Ir_ext_call (mf, args)
  | Ir_field (idx, e) -> Ir_field (idx, rename path e)
  | Ir_fun (args, body) -> Ir_fun (args, rename path body)
  | Ir_letrec (bindings, e) ->
      let bindings = List.map (fun (id, e2) -> (id, rename path e2)) bindings in
      let e = rename path e in
      Ir_letrec (bindings, e)
  | Ir_record fields ->
      let fields = List.map (fun (idx, e) -> (idx, rename path e)) fields in
      Ir_record fields
  | Ir_throw (idx, exprs) -> Ir_throw (idx, List.map (rename path) exprs)
  | Ir_seq (e1, e2) -> Ir_seq (rename path e1, rename path e2)
  | Ir_program parts -> Ir_program (List.map (rename path) parts)
  | Ir_lit _ | Ir_var _ -> ir

let run ir = rename [] ir
