(**

   Rewrite local function references that from `Ir_var` to `Ir_fn_name`. This
   pass works similarly to `pass_ext_calls` except that it works within a single
   module.

   To do this we will build a table of the local functions in the module, and
   every time we find an identifier that should be converted into a funref,
   we'll look it up in the function table. If it is one, we'll turn it into a
   funref.

*)

open Ir

module Local_fun_table = struct
  open Sexplib.Std

  let build ir =
    let tbl = Hashtbl.create 1024 in

    let arity expr =
      match expr with Ir_fun (args, _) -> List.length args | _ -> -1
    in

    let rec aux ir =
      let open Identifier in
      match ir with
      | Ir_module (_mod_name, next) -> aux next
      | Ir_letrec (bindings, next) ->
          List.iter
            (fun (_v, name, body) ->
              Hashtbl.add tbl name.unique_name (arity body);
              aux body)
            bindings;
          aux next
      | Ir_let (_v, name, body, next) ->
          let arity = arity body in
          Hashtbl.add tbl name.unique_name arity;
          aux body;
          aux next
      | Ir_program parts -> List.iter aux parts
      | _ -> ()
    in
    aux ir;
    tbl

  let find tbl m = 
    match Hashtbl.find_opt tbl Identifier.(m.unique_name) with
    | Some -1 | None -> None
    | Some arity -> Some arity
end

let rec replace ~to_funref tbl ir =
  match ir with
  | Ir_var name when to_funref -> (
      match Local_fun_table.find tbl name with
      | Some arity -> Ir.fn_name ~name ~arity
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
  let tbl = Local_fun_table.build ir in
  replace ~to_funref:false tbl ir
