module Erl = Erlang.Parsetree_helper

module Curry = struct end

(* This module translates function cases from OCaml to Erlang.
 *)
module Function_case = struct
  let make_one case =
    let lhs = Pattern.from_ocaml_case ~case in
    let rhs = Expression.from_ocaml_case ~case in
    Erl.Case.mk ~lhs ~rhs

  let make ~cases = List.map make_one cases
end

let export_from_signature ~name ~value_desc:_ =
  let name = Identifier.function_name name in
  Erl.Attr.export ~name ~arity:1

let make ~ident ~cases =
  let name = Identifier.function_name ident in
  Erl.Fun_decl.mk ~name ~arity:1 ~cases:(Function_case.make ~cases)

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

  let find_functions ~structure =
    List.concat_map
      (function
        | { str_desc = Tstr_value (_, bindings); _ } ->
            List.map handle_binding bindings
        | _ -> [])
      structure.str_items
end

let from_ocaml ~structure =
  let all_functions = Function_finder.find_functions ~structure in
  List.map (fun (ident, cases) -> make ~ident ~cases) all_functions
