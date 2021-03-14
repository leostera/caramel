type atom = Atom of string

and comment = Comment of string

and guard = expr list

and name =
  | Var_name of string
  | Atom_name of atom
  | Qualified_name of { n_mod : name; n_name : name }

and map_field = { mf_name : expr; mf_value : expr }

and case = { c_lhs : pattern list; c_guard : guard option; c_rhs : expr }

and let_binding = { lb_lhs : pattern; lb_rhs : expr }

and literal =
  | Lit_integer of string
  | Lit_char of string
  | Lit_binary of string
  | Lit_string of string
  | Lit_float of string
  | Lit_atom of atom

and recv = { rcv_cases : case list; rcv_after : case option }

and catch_class = Class_error | Class_throw

and try_catch = {
  try_expr : expr;
  try_catch : case list option;
  try_after : expr option;
}

and expr =
  | Expr_apply of fun_apply
  | Expr_case of expr * case list
  | Expr_catch of expr
  | Expr_comment of comment * expr
  | Expr_cons of expr list * expr
  | Expr_fun of case list
  | Expr_fun_ref of { fref_name : name; fref_arity : int }
  | Expr_if of (expr list list * expr) list
  | Expr_let of let_binding * expr
  | Expr_list of expr list
  | Expr_literal of literal
  | Expr_macro of string
  | Expr_map of map_field list
  | Expr_map_update of expr * map_field list
  | Expr_name of name
  | Expr_nil
  | Expr_recv of recv
  | Expr_seq of expr list
  | Expr_try of try_catch
  | Expr_tuple of expr list

and pattern =
  | Pattern_binding of name
  | Pattern_catch of name option * pattern * name option
  | Pattern_cons of pattern list * pattern
  | Pattern_ignore
  | Pattern_list of pattern list
  | Pattern_map of (pattern * pattern) list
  | Pattern_match of literal
  | Pattern_tuple of pattern list
  | Pattern_with_name of pattern * pattern

and fun_apply = { fa_name : expr; fa_args : expr list }

and fun_decl = {
  fd_name : atom;
  fd_arity : int;
  fd_cases : case list;
  fd_spec : type_expr option;
}

and record_field = { rf_name : atom; rf_type : type_expr }

and type_constr = { tc_name : name; tc_args : type_expr list }

and field_presence = Optional | Mandatory

and type_map_field = {
  tmf_name : type_expr;
  tmf_presence : field_presence;
  tmf_value : type_expr;
}

and type_expr =
  | Type_function of { tyfun_args : type_expr list; tyfun_return : type_expr }
  | Type_constr of type_constr
  | Type_variable of name
  | Type_tuple of type_expr list
  | Type_list of type_expr
  | Type_record of (name * record_field list)
  | Type_map of type_map_field list
  | Type_variant of type_expr list
  | Type_const of literal

and type_kind = Opaque | Type | Spec | Callback

and type_decl = {
  typ_expr : type_expr;
  typ_kind : type_kind;
  typ_name : atom;
  typ_params : type_expr list;
}

(** An exported symbol in an Erlang module. This could be a function or a type.
    See:
      http://erlang.org/doc/reference_manual/modules.html for missing fields.
      http://erlang.org/doc/reference_manual/typespec.html
 *)
and export_type = Export_function | Export_type

and export = { exp_type : export_type; exp_name : atom; exp_arity : int }

and attribute = { atr_name : atom; atr_value : expr }

and module_item =
  | Module_comment of comment
  | Module_attribute of attribute
  | Type_decl of type_decl
  | Function_decl of fun_decl

and structure = module_item list

and t = {
  file_name : string;
  behaviours : atom list;
  module_name : atom;
  attributes : attribute list;
  exports : export list;
  types : type_decl list;
  functions : fun_decl list;
}

val sexp_of_t : t -> Sexplib.Sexp.t

val sexp_of_structure : structure -> Sexplib.Sexp.t

val sexp_of_expr : expr -> Sexplib.Sexp.t
