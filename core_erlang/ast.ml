type atom = string

type var_name = string

type literal =
  | Lit_integer of int
  | Lit_float of float
  | Lit_char of char
  | Lit_string of string
  | Lit_atom of atom
  | Lit_nil
  | Lit_cons of literal * literal (* improper lists are a thing! *)
  | Lit_tuple of literal list

and pattern =
  | Pat_var_name of var_name
  | Pat_tuple of pattern list
  | Pat_list of pattern list
  | Pat_bitstring of pattern list

and let_binding = { lb_lhs : pattern; lb_expr : expr; lb_rhs : expr }

and letrec_binding = { lrb_lhs : (pattern * expr) list; lrb_rhs : expr }

and clause = { cp_lhs : pattern; cp_guard : expr option; cp_rhs : expr }

and case_expr = { case_exp : expr; case_pat : clause list }

and fun_expr = { fe_vars : var_name list; fe_arity : int; fe_body : expr }

and receive_expr = { rcv_pat : clause list; tm_after : expr; tm_body : expr }

and try_catch_expr = {
  tc_exp : expr;
  tc_vars : var_name list;
  tc_in : expr;
  tc_catch_vars : var_name list;
  tc_catch : expr;
}

and expr =
  | Expr_var of var_name
  | Expr_literal of literal
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

type fname = { fn_name : atom; fn_arity : int }

type fun_def = { fd_name : fname; fd_body : fun_expr }

type attribute = { atr_name : atom; atr_value : literal }

type t = {
  m_filename : string;
  m_name : atom;
  m_fnames : fname list;
  m_attributes : attribute list;
  m_defs : fun_def list;
}
