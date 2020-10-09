open Sexplib.Std

type atom = Atom of string [@@deriving sexp]

and comment = Comment of string [@@deriving_sexp]

and guard = expr list [@@deriving sexp]

and name =
  | Var_name of string
  | Atom_name of atom
  | Qualified_name of { n_mod : name; n_name : name }
[@@deriving sexp]

and map_field = { mf_name : expr; mf_value : expr } [@@deriving sexp]

and case = { c_lhs : pattern list; c_guard : guard option; c_rhs : expr }
[@@deriving sexp]

and let_binding = { lb_lhs : pattern; lb_rhs : expr } [@@deriving sexp]

and literal =
  | Lit_integer of string
  | Lit_char of string
  | Lit_binary of string
  | Lit_string of string
  | Lit_float of string
  | Lit_atom of atom
[@@deriving sexp]

and recv = { rcv_cases : case list; rcv_after : case option } [@@deriving sexp]

and catch_class = Class_error | Class_throw [@@deriving sexp]

and try_catch = {
  try_expr : expr;
  try_catch : case list option;
  try_after : expr option;
}
[@@deriving sexp]

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
  | Expr_try of try_catch
  | Expr_tuple of expr list
[@@deriving sexp]

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
[@@deriving sexp]

and fun_apply = { fa_name : expr; fa_args : expr list } [@@deriving sexp]

and fun_decl = {
  fd_name : atom;
  fd_arity : int;
  fd_cases : case list;
  fd_spec : type_expr option;
}
[@@deriving sexp]

(** A type declaration in an Erlang module. This follows what is currently
    representable by Dialyzer.

    See:
      http://erlang.org/doc/reference_manual/typespec.html
 *)

and record_field = { rf_name : atom; rf_type : type_expr } [@@deriving sexp]

and type_constr = { tc_name : name; tc_args : type_expr list } [@@deriving sexp]

and field_presence = Optional | Mandatory [@@deriving sexp]

and type_map_field = {
  tmf_name : type_expr;
  tmf_presence : field_presence;
  tmf_value : type_expr;
}
[@@deriving sexp]

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
[@@deriving sexp]

and type_kind = Opaque | Type | Spec | Callback [@@deriving sexp]

and type_decl = {
  typ_expr : type_expr;
  typ_kind : type_kind;
  typ_name : atom;
  typ_params : type_expr list;
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
  | Module_comment of comment
  | Module_attribute of attribute
  | Type_decl of type_decl
  | Function_decl of fun_decl
[@@deriving sexp]

and structure = module_item list [@@deriving sexp]

and t = {
  file_name : string;
  behaviours : atom list;
  module_name : atom;
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
