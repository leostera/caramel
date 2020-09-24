open Sexplib.Std

type atom = string [@@deriving sexp]

and var_name = string [@@deriving sexp]

and guard = unit [@@deriving sexp]

and name =
  | Var_name of atom
  | Atom_name of atom
  | Macro_name of atom
  | Qualified_name of { n_mod : atom; n_name : atom }
[@@deriving sexp]

and map_field = { mf_name : atom; mf_value : expr } [@@deriving sexp]

and case_branch = { cb_pattern : pattern; cb_expr : expr } [@@deriving sexp]

and let_binding = { lb_lhs : pattern; lb_rhs : expr } [@@deriving sexp]

and literal =
  | Lit_integer of string
  | Lit_char of string
  | Lit_binary of string
  | Lit_float of string
  | Lit_atom of string
[@@deriving sexp]

and expr =
  | Expr_let of let_binding * expr
  | Expr_name of name
  | Expr_literal of literal
  (* NOTE: function references will be resolved at print time for now :( *)
  | Expr_fun_ref of atom
  | Expr_apply of fun_apply
  | Expr_map of map_field list
  | Expr_list of expr list
  | Expr_case of expr * case_branch list
  | Expr_tuple of expr list
  | Expr_fun of fun_decl
[@@deriving sexp]

and pattern =
  | Pattern_ignore
  | Pattern_binding of atom
  | Pattern_tuple of pattern list
  | Pattern_list of pattern list
  | Pattern_map of (atom * pattern) list
  | Pattern_match of literal
[@@deriving sexp]

and fun_apply = { fa_name : expr; fa_args : expr list } [@@deriving sexp]

and fun_case = {
  fc_name : atom;
  fc_lhs : pattern list;
  fc_guards : guard list;
  fc_rhs : expr;
}
[@@deriving sexp]

and fun_decl = { fd_name : atom; fd_arity : int; fd_cases : fun_case list }
[@@deriving sexp]

(** A type declaration in an Erlang module. This follows what is currently
    representable by Dialyzer.

    See:
      http://erlang.org/doc/reference_manual/typespec.html
 *)

and record_field = { rf_name : atom; rf_type : type_kind } [@@deriving sexp]

and variant_constructor = Constructor of type_constr | Extension of type_kind
[@@deriving sexp]

and type_constr = { tc_name : name; tc_args : type_kind list } [@@deriving sexp]

and type_kind =
  | Type_function of type_kind list
  | Type_constr of type_constr
  | Type_variable of atom
  | Type_tuple of type_kind list
  | Type_record of { fields : record_field list }
  | Type_variant of { constructors : variant_constructor list }
[@@deriving sexp]

and type_visibility = Opaque | Visible [@@deriving sexp]

and type_decl = {
  typ_kind : type_kind;
  typ_visibility : type_visibility;
  typ_name : atom;
  typ_params : var_name list;
}
[@@deriving sexp]

(** An exported symbol in an Erlang module. This could be a function or a type.
    See:
      http://erlang.org/doc/reference_manual/modules.html for missing fields.
      http://erlang.org/doc/reference_manual/typespec.html
 *)
and export_type = Export_function | Export_type [@@deriving sexp]

and export = { exp_type : export_type; exp_name : atom; exp_arity : int }
[@@deriving sexp]

and attribute = { atr_name : atom; atr_value : expr } [@@deriving sexp]

and module_item =
  | Module_attribute of attribute
  | Type_decl of type_decl
  | Function_decl of fun_decl
[@@deriving sexp]

and t = {
  file_name : string;
  behaviours : atom list;
  module_name : atom;
  ocaml_name : atom;
  attributes : attribute list;
  exports : export list;
  types : type_decl list;
  functions : fun_decl list;
}
[@@deriving sexp]

(** The type of an Erlang module. Intentionally incomplete for now.
    See:
      http://erlang.org/doc/reference_manual/modules.html
 *)

let empty =
  {
    file_name = "empty.erl";
    behaviours = [];
    module_name = "empty";
    ocaml_name = "empty";
    attributes = [];
    exports = [];
    types = [];
    functions = [];
  }

let rec item_list_to_module items acc =
  match items with
  | [] -> acc
  | x :: xs ->
      item_list_to_module xs
        ( match x with
        | Module_attribute
            {
              atr_name = "module";
              atr_value = Expr_literal (Lit_atom module_name);
            } ->
            {
              acc with
              module_name;
              file_name = module_name ^ ".erl";
              ocaml_name = String.capitalize_ascii module_name;
            }
        | Module_attribute
            {
              atr_name = "behavior";
              atr_value = Expr_literal (Lit_atom behavior);
            }
        | Module_attribute
            {
              atr_name = "behaviour";
              atr_value = Expr_literal (Lit_atom behavior);
            } ->
            { acc with behaviours = behavior :: acc.behaviours }
        | Module_attribute { atr_name = "export"; atr_value = Expr_list attrs }
          ->
            {
              acc with
              exports =
                ( attrs
                |> List.filter_map (function
                     | Expr_tuple
                         [
                           Expr_literal (Lit_atom exp_name);
                           Expr_literal (Lit_integer exp_arity);
                         ] ->
                         Some
                           {
                             exp_type = Export_function;
                             exp_name;
                             exp_arity = int_of_string exp_arity;
                           }
                     | _ -> None) )
                @ acc.exports;
            }
        | Module_attribute
            { atr_name = "export_type"; atr_value = Expr_list attrs } ->
            {
              acc with
              exports =
                ( attrs
                |> List.filter_map (function
                     | Expr_tuple
                         [
                           Expr_literal (Lit_atom exp_name);
                           Expr_literal (Lit_integer exp_arity);
                         ] ->
                         Some
                           {
                             exp_type = Export_type;
                             exp_name;
                             exp_arity = int_of_string exp_arity;
                           }
                     | _ -> None) )
                @ acc.exports;
            }
        | Module_attribute atr ->
            { acc with attributes = atr :: acc.attributes }
        | Type_decl td -> { acc with types = td :: acc.types }
        | Function_decl fd -> { acc with functions = fd :: acc.functions } )

let of_module_items items =
  match items with
  | [] -> Error `Module_item_list_was_empty
  | [
   Module_attribute
     { atr_name = "module"; atr_value = Expr_literal (Lit_atom module_name) };
  ] ->
      Ok
        {
          empty with
          module_name;
          file_name = module_name ^ ".erl";
          ocaml_name = String.capitalize_ascii module_name;
        }
  | [ _ ] -> Error `Single_module_item_was_not_a_module_name
  | xs -> Ok (item_list_to_module xs empty)

let make ~name ~ocaml_name ~exports ~types ~functions ~attributes =
  {
    file_name = name ^ ".erl";
    behaviours = [];
    module_name = name;
    ocaml_name;
    attributes;
    exports;
    types;
    functions;
  }

let make_fn_export exp_name exp_arity =
  { exp_type = Export_function; exp_name; exp_arity }

let make_type_export exp_name exp_arity =
  { exp_type = Export_type; exp_name; exp_arity }

let make_named_type typ_name typ_params typ_kind typ_visibility =
  let used_params : atom list =
    let rec collect_params acc k =
      let flat_args =
        match k with
        | Type_function args -> List.concat_map (collect_params []) args
        | Type_constr { tc_args; _ } ->
            tc_args |> List.concat_map (collect_params [])
        | Type_variable atom -> [ atom ]
        | Type_tuple args -> List.concat_map (collect_params []) args
        | Type_record { fields } ->
            fields
            |> List.map (fun { rf_type; _ } -> rf_type)
            |> List.concat_map (collect_params [])
        | Type_variant { constructors } ->
            constructors
            |> List.concat_map (function
                 | Constructor { tc_args; _ } ->
                     List.concat_map (collect_params []) tc_args
                 | Extension ext -> collect_params [] ext)
      in
      [ flat_args; acc ] |> List.concat
    in
    collect_params [] typ_kind
  in
  let typ_params =
    typ_params
    |> List.map (fun p ->
           if List.exists (fun up -> up = p) used_params then p else "_" ^ p)
  in
  { typ_name; typ_params; typ_kind; typ_visibility }

let type_any = Type_constr { tc_name = Atom_name "any"; tc_args = [] }

let find_fun_by_name ~module_ name =
  module_.functions |> List.find_opt (fun { fd_name; _ } -> fd_name = name)
