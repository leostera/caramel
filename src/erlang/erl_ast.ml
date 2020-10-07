open Sexplib.Std

type atom = Atom of string [@@deriving sexp]

and comment = Comment of string [@@deriving_sexp]

and guard = unit [@@deriving sexp]

and name =
  | Var_name of string
  | Atom_name of atom
  | Qualified_name of { n_mod : atom; n_name : atom }
[@@deriving sexp]

and map_field = { mf_name : atom; mf_value : expr } [@@deriving sexp]

and case = { c_lhs : pattern list; c_guard : guard option; c_rhs : expr }
[@@deriving sexp]

and let_binding = { lb_lhs : pattern; lb_rhs : expr } [@@deriving sexp]

and literal =
  | Lit_integer of string
  | Lit_char of string
  | Lit_binary of string
  | Lit_float of string
  | Lit_atom of atom
[@@deriving sexp]

and recv = { rcv_cases : case list; rcv_after : case option } [@@deriving sexp]

and expr =
  | Expr_let of let_binding * expr
  | Expr_name of name
  | Expr_literal of literal
  | Expr_fun_ref of { fref_name : atom; fref_arity : int }
  | Expr_apply of fun_apply
  | Expr_recv of recv
  | Expr_map of map_field list
  | Expr_list of expr list
  | Expr_nil
  | Expr_cons of expr list * expr
  | Expr_case of expr * case list
  | Expr_tuple of expr list
  | Expr_fun of case list
[@@deriving sexp]

and pattern =
  | Pattern_ignore
  | Pattern_binding of name
  | Pattern_tuple of pattern list
  | Pattern_list of pattern list
  | Pattern_cons of pattern list * pattern
  | Pattern_map of (atom * pattern) list
  | Pattern_match of literal
[@@deriving sexp]

and fun_apply = { fa_name : expr; fa_args : expr list } [@@deriving sexp]

and fun_decl = {
  fd_name : atom;
  fd_arity : int;
  fd_cases : case list;
  fd_spec : type_kind option;
}
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
  | Type_function of { tyfun_args : type_kind list; tyfun_return : type_kind }
  | Type_constr of type_constr
  | Type_variable of name
  | Type_tuple of type_kind list
  | Type_record of { tyrec_fields : record_field list }
  | Type_variant of { tyvar_constructors : variant_constructor list }
[@@deriving sexp]

and type_visibility = Opaque | Visible [@@deriving sexp]

and type_decl = {
  typ_kind : type_kind;
  typ_visibility : type_visibility;
  typ_name : atom;
  typ_params : name list;
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
