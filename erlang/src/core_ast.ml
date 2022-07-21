open Sexplib.Std

type atom = string [@@deriving sexp]

type var_name = string [@@deriving sexp]

type literal =
  | Lit_integer of int
  | Lit_float of float
  | Lit_char of char
  | Lit_string of string
  | Lit_atom of atom
  | Lit_nil
  | Lit_cons of literal list * literal (* improper lists are a thing! *)
  | Lit_tuple of literal list
  | Lit_list of literal list
[@@deriving sexp]

and pattern =
  | Pat_lit of literal
  | Pat_var_name of var_name
  | Pat_tuple of pattern list
  | Pat_list of pattern list
  | Pat_cons of pattern list * pattern
  | Pat_bitstring of bitstring_pattern list
  | Pat_alias of var_name * pattern
[@@deriving sexp]

and bitstring = { bits_lhs : expr; bits_rhs : expr list }
[@@deriving sexp]

and bitstring_pattern = { bits_pat_lhs : pattern; bits_pat_rhs : expr list }
[@@ deriving sexp]

and let_binding = { lb_lhs : var_name list; lb_expr : expr; lb_rhs : expr }
[@@deriving sexp]

and letrec_binding = { lrb_lhs : fun_def list; lrb_rhs : expr }
[@@deriving sexp]

and clause = { cp_lhs : pattern list; cp_guard : expr; cp_rhs : expr }
[@@deriving sexp]

and case_expr = { case_exp : expr; case_pat : clause list } [@@deriving sexp]

and fun_expr = { fe_vars : var_name list; fe_body : expr }
[@@deriving sexp]

and receive_expr = { rcv_pat : clause list; tm_after : expr; tm_body : expr }
[@@deriving sexp]

and try_catch_expr = {
  tc_exp : expr;
  tc_vars : var_name list;
  tc_in : expr;
  tc_catch_vars : var_name list;
  tc_catch : expr;
}
[@@deriving sexp]

and expr =
  | Expr_var of var_name
  | Expr_literal of literal
  | Expr_val_list of expr list
  | Expr_fname of fname
  | Expr_tuple of expr list
  | Expr_list of expr list
  | Expr_cons of expr list * expr
  | Expr_binary of bitstring list
  | Expr_let of let_binding
  | Expr_letrec of letrec_binding
  | Expr_case of case_expr
  | Expr_apply of { fn_name : expr; fn_args : expr list }
  (* In the specification these are called "InterModuleCalls" *)
  | Expr_qualified_call of { qc_mod : expr; qc_fun : expr; qc_args : expr list }
  | Expr_fun of fun_expr
  | Expr_receive of receive_expr
  | Expr_primop of { pop_name : atom; pop_args : expr list }
  | Expr_try of try_catch_expr
  | Expr_do of expr list
  | Expr_catch of expr
[@@deriving sexp]

and fname = { fn_name : atom; fn_arity : int } [@@deriving sexp]

and fun_def = { fd_name : fname; fd_body : fun_expr } [@@deriving sexp]

and attribute = { atr_name : atom; atr_value : literal } [@@deriving sexp]

type t = {
  m_filename : string;
  m_name : atom;
  m_fnames : fname list;
  m_attributes : attribute list;
  m_defs : fun_def list;
}
[@@deriving sexp]
