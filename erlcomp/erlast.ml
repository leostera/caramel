type atom = string

and guard = unit

and name =
  | Var_name of atom
  | Atom_name of atom
  | Macro_name of atom
  | Qualified_name of { n_mod: atom; n_name: atom }

and map_field = {
  mf_name: atom;
  mf_value: expr
}

and case_branch = {
  cb_pattern: pattern;
  cb_expr: expr
}

and let_binding = {
  lb_lhs: pattern;
  lb_rhs: expr;
}

and literal =
  | Lit_integer of string
  | Lit_char of string
  | Lit_binary of string
  | Lit_float of string
  | Lit_atom of string

and expr =
  | Expr_let of let_binding * expr
  | Expr_name of name
  | Expr_literal of literal
  (* NOTE: function references will be resolved at print time for now :( *)
  | Expr_fun_ref of atom
  | Expr_apply of fun_apply
  | Expr_map of map_field list
  | Expr_list of expr list
  | Expr_case of expr * (case_branch list)
  | Expr_tuple of expr list
  | Expr_fun of fun_decl


and pattern =
  | Pattern_ignore
  | Pattern_binding of atom
  | Pattern_tuple of pattern list
  | Pattern_list of pattern list
  | Pattern_map of (atom * pattern) list
  | Pattern_match of literal

and fun_apply = {
  fa_name: expr;
  fa_args: expr list
}

and fun_case = {
  fc_lhs: pattern list;
  fc_guards: guard list;
  fc_rhs: expr;
}

and fun_decl = {
  fd_name: atom;
  fd_arity: int;
  fd_cases: fun_case list;
}

(** A type declaration in an Erlang module. This follows what is currently
    representable by Dialyzer.

    See:
      http://erlang.org/doc/reference_manual/typespec.html
 *)

and record_field = { rf_name: atom; rf_type: type_kind }

and variant_constructor =
  | Constructor of { vc_name: atom; vc_args: type_kind list }
  | Extension of type_kind

and type_constr = { tc_name: name; tc_args: type_kind list }

and type_kind =
  | Type_function of type_kind list
  | Type_constr of type_constr
  | Type_variable of atom
  | Type_tuple of type_kind list
  | Type_record of { fields: record_field list; }
  | Type_variant of { constructors: variant_constructor list; }

and type_decl = {
  typ_kind: type_kind;
  typ_name: atom;
  typ_params: atom list;
}

(** An exported symbol in an Erlang module. This could be a function or a type.
    See:
      http://erlang.org/doc/reference_manual/modules.html for missing fields.
      http://erlang.org/doc/reference_manual/typespec.html
 *)
and export_type = Export_function | Export_type

and export = {
  exp_type: export_type;
  exp_name: atom;
  exp_arity: int;
}

(** The type of an Erlang module. Intentionally incomplete for now.
    See:
      http://erlang.org/doc/reference_manual/modules.html
 *)
and t = {
  file_name: string;
  behaviour: atom option;
  module_name: atom;
  ocaml_name: atom;
  exports: export list;
  types: type_decl list;
  functions: fun_decl list;
}

let make ~name ~ocaml_name ~exports ~types ~functions = {
  file_name = name ^ ".erl";
  behaviour = None;
  module_name = name;
  ocaml_name = ocaml_name;
  exports = exports;
  types = types;
  functions = functions;
}

let make_fn_export exp_name exp_arity = {exp_type=Export_function; exp_name; exp_arity }
let make_type_export exp_name exp_arity = {exp_type=Export_type; exp_name; exp_arity }

let make_named_type typ_name typ_params typ_kind =
  let used_params: atom list =
    let rec collect_params acc k =
      let flat_args = match k with
      | Type_function args ->
          List.concat_map (collect_params []) args

      | Type_constr { tc_args }  ->
          tc_args
          |> List.concat_map (collect_params [])

      | Type_variable atom -> [atom]

      | Type_tuple args ->
          List.concat_map (collect_params []) args

      | Type_record  { fields } ->
          fields
          |> List.map (fun { rf_type } -> rf_type )
          |> List.concat_map (collect_params [] )

      | Type_variant { constructors } ->
          constructors
          |> List.concat_map (function
              | (Constructor { vc_args }) -> vc_args
              | _ -> [] )
          |> List.concat_map (collect_params [])
      in
      [flat_args; acc] |> List.concat
    in
    collect_params [] typ_kind
  in
  let typ_params = typ_params |> List.map (fun p ->
    if List.exists (fun up -> up = p) used_params
    then p
    else "_" ^ p
  ) in
  { typ_name; typ_params; typ_kind }

let type_any = Type_constr { tc_name=Atom_name "any"; tc_args=[] }

let find_fun_by_name ~module_ name =
  module_.functions |> List.find_opt (fun { fd_name } -> fd_name = name )
