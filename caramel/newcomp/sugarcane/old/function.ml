module Curry = struct end

(* This module translates function cases from OCaml to Erlang.
 *)
module Function_case = struct
  let make_one ~ctx case =
    let _lhs = Pattern.from_ocaml_case ~ctx ~case in
    let _rhs = Expression.from_ocaml_case ~ctx ~case in
    (* TODO: build a Ir.case_desc instead
       Erl.Case.mk ~lhs ~rhs
    *)
    Error.todo "Function_case.make_one"

  let make ~ctx ~cases = List.map (make_one ~ctx) cases
end

let export_from_signature ~ident ~value_desc:_ =
  let _ident = Identifier.from_ident ~ident in
  (* TODO: build an Ir.attr_desc instead
     let name = Identifier.export ident in
        Erl.Attr.export ~name ~arity:1
  *)
  Error.todo "export_from_signature"

let make ~ctx:_ ~ident ~cases:_ =
  let _ident = Identifier.from_ident ~ident in
  (* TODO: build an Ir.fun_desc instead
     let name = Identifier.local_symbol ident in
          Erl.Fun_decl.mk ~name ~arity:1 ~cases:(Function_case.make ~ctx ~cases)
  *)
  Error.todo "Function.make"

(* Find all top-level functions in an OCaml module structure.
 *)
module Function_finder = struct
  open Typedtree

  let is_function_declaration vb =
    match (vb.vb_pat.pat_desc, vb.vb_expr.exp_desc) with
    | Tpat_var (id, _), Texp_function { cases; _ } -> `fun_decl (id, cases)
    | _ -> `not_fun_decl

  let handle_binding value_binding =
    match is_function_declaration value_binding with
    | `fun_decl (ident, cases) -> (ident, cases)
    | `not_fun_decl -> Error.unsupported_top_level_module_value ()

  let find_functions ~transl_input =
    List.concat_map
      (function
        | { str_desc = Tstr_value (_, bindings); _ } ->
            List.map handle_binding bindings
        | _ -> [])
      Translation_input.(transl_input.caml_structure.str_items)
end

let from_ocaml ~ctx transl_input =
  let all_functions = Function_finder.find_functions ~transl_input in
  List.map (fun (ident, cases) -> make ~ctx ~ident ~cases) all_functions
