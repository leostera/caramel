%{
open Core_ast

%}

(* Constant literals, variables *)
%token <string * Parse_info.t> ATOM STRING VARIABLE
%token <int * Parse_info.t> INTEGER
%token <float * Parse_info.t> FLOAT
%token <char * Parse_info.t> CHAR

(* Keywords *)
%token <Parse_info.t> AFTER APPLY ATTRIBUTES CALL CASE CATCH DO END FUN IN LET LETREC MODULE OF PRIMOP RECEIVE TRY WHEN

(* Separators *)
%token <Parse_info.t> LEFT_PARENS RIGHT_PARENS LEFT_BRACE RIGHT_BRACE LEFT_BRACKET RIGHT_BRACKET LEFT_ANGLE RIGHT_ANGLE PIPE HASH COMMA COLON SLASH EQUAL ARROW DASH_PIPE

(* End of file *)
%token <Parse_info.t> EOF

%start <Core_ast.t> annotated_module
%%

(* TODO: add annotations to the ast *)
let annotated_module :=
    | m = module_t; EOF; { m }
    | LEFT_PARENS; m = module_t; DASH_PIPE; LEFT_BRACKET; _ = separated_list(COMMA, constant); RIGHT_BRACKET; EOF; { m }

let module_t :=
    | MODULE; m_name = atom; (m_fnames, m_attributes) = module_header; m_defs = module_body; END; { { m_filename = (m_name ^ ".core"); m_name; m_fnames; m_attributes; m_defs } }

let module_header :=
    | ~ = exports; ~ = attributes; { (exports, attributes) }

let exports :=
    | LEFT_BRACKET; fnames = separated_list(COMMA, function_name); RIGHT_BRACKET; { fnames }

let function_name :=
    | fn_name = atom; SLASH; fn_arity = integer; { { fn_name; fn_arity } }

let attributes :=
    | ATTRIBUTES; LEFT_BRACKET; attributes = separated_list(COMMA, module_attribute); RIGHT_BRACKET; { attributes }

let module_attribute :=
    | atr_name = atom; EQUAL; atr_value = constant; { { atr_name; atr_value; } }

let module_body :=
    | fds = function_definition*; { fds }

let function_definition :=
    | fd_name = annotated_function_name; EQUAL; fd_body = annotated_fun; { { fd_name; fd_body } }

let annotated_function_name :=
    | fn = function_name; { fn }
    | LEFT_PARENS; fn = function_name; DASH_PIPE; LEFT_BRACKET; _ = separated_list(COMMA, constant); RIGHT_BRACKET; RIGHT_PARENS; { fn }

let annotated_fun :=
    | f = fun_t; { f }
    | LEFT_PARENS; f = fun_t; DASH_PIPE; LEFT_BRACKET; _ = separated_list(COMMA, constant); RIGHT_BRACKET; RIGHT_PARENS; { f }

let constant :=
    | lit = atomic_literal; { lit }
    | LEFT_BRACE; lits = separated_list(COMMA, constant); RIGHT_BRACE; { Lit_tuple lits }
    | LEFT_BRACKET; lits = separated_nonempty_list(COMMA, constant); RIGHT_BRACKET; { Lit_list lits }
    | LEFT_BRACKET; lhs = separated_nonempty_list(COMMA, constant); PIPE; rhs = constant; RIGHT_BRACKET; { Lit_cons (lhs, rhs) }

let atomic_literal :=
   | (i, _) = INTEGER; { Lit_integer i }
   | (f, _) = FLOAT; { Lit_float f }
   | (a, _) = ATOM; { Lit_atom a }
   | LEFT_BRACKET; RIGHT_BRACKET; { Lit_nil }
   | (c, _) = CHAR; { Lit_char c }
   | (s, _) = STRING; { Lit_string s }

let variable_name :=
   | (n, _) = VARIABLE; { n }

let annotated_variable :=
    | v = variable_name; { v }
    | LEFT_PARENS; v = variable_name; DASH_PIPE; LEFT_BRACKET; _ = separated_list(COMMA, constant); RIGHT_BRACKET; RIGHT_PARENS; { v }

let annotated_pattern :=
    | v = annotated_variable; { Pat_var_name v }
    | p = pattern; { p }
    | LEFT_PARENS; p = pattern; DASH_PIPE; LEFT_BRACKET; _ = separated_list(COMMA, constant); RIGHT_BRACKET; RIGHT_PARENS; { p }

let pattern :=
    | lit = atomic_literal; { Pat_lit lit }
    | LEFT_BRACE; ps = separated_list(COMMA, annotated_pattern); RIGHT_BRACE; { Pat_tuple ps }
    | LEFT_BRACKET; ps = separated_nonempty_list(COMMA, annotated_pattern); RIGHT_BRACKET; { Pat_list ps }
    | LEFT_BRACKET; lhs = separated_nonempty_list(COMMA, annotated_pattern); PIPE; rhs = annotated_pattern; RIGHT_BRACKET; { Pat_cons (lhs, rhs) }
    | HASH; LEFT_BRACE; ps = separated_list(COMMA, bitstring_pattern); RIGHT_BRACE; HASH; { Pat_bitstring ps }
    | v = annotated_variable; EQUAL; p = annotated_pattern; { Pat_alias (v, p) }

let bitstring_pattern :=
    | HASH; LEFT_ANGLE; bits_pat_lhs = annotated_pattern; RIGHT_ANGLE; LEFT_PARENS; bits_pat_rhs = separated_list(COMMA, expression); RIGHT_PARENS; { { bits_pat_lhs; bits_pat_rhs } }

let expression :=
    | e = annotated_value_list; { e }
    | e = annotated_single_expression; { e }

let annotated_value_list :=
    | vlist = value_list; { vlist }
    | LEFT_PARENS; vlist = value_list; DASH_PIPE; LEFT_BRACKET; _ = separated_list(COMMA, constant); RIGHT_BRACKET; RIGHT_PARENS; { vlist }

let value_list :=
    | LEFT_ANGLE; es = separated_list(COMMA, annotated_single_expression); RIGHT_ANGLE; { Expr_val_list es }

let annotated_single_expression :=
    | e = single_expression; { e }
    | LEFT_PARENS; e = single_expression; DASH_PIPE; LEFT_BRACKET; _ = separated_list(COMMA, constant); RIGHT_BRACKET; RIGHT_PARENS; { e }

let single_expression :=
    | e = atomic_literal; { Expr_literal e }
    | e = variable_name; { Expr_var e }
    | e = function_name; { Expr_fname e }
    | e = tuple; { Expr_tuple e }
    | e = list_t; { e } (* Expr_list or Expr_cons *)
    | e = binary; { Expr_binary e }
    | e = let_t; { Expr_let e }
    | e = case; { Expr_case e }
    | e = fun_t; { Expr_fun e }
    | e = letrec; { Expr_letrec e }
    | (fn_name, fn_args) = application; { Expr_apply { fn_name; fn_args } }
    | (qc_mod, qc_fun, qc_args) = inter_module_call; { Expr_qualified_call { qc_mod; qc_fun; qc_args } }
    | (pop_name, pop_args) = prim_op_call; { Expr_primop { pop_name; pop_args } }
    | e = try_t; { Expr_try e }
    | e = receive; { Expr_receive e }
    | e = sequencing; { Expr_do e }
    | e = catch; { Expr_catch e }

let tuple :=
    | LEFT_BRACE; es = separated_list(COMMA, expression); RIGHT_BRACE; { es }

let list_t :=
    | LEFT_BRACKET; es = separated_nonempty_list(COMMA, expression); RIGHT_BRACKET; { Expr_list es }
    | LEFT_BRACKET; lhs = separated_nonempty_list(COMMA, expression); PIPE; rhs = expression; RIGHT_BRACKET; { Expr_cons (lhs, rhs) }

let binary :=
    | HASH; LEFT_BRACE; bitstrings = separated_list(COMMA, bitstring); RIGHT_BRACE; HASH; { bitstrings }

let bitstring :=
    | HASH; LEFT_ANGLE; bits_lhs = expression; RIGHT_ANGLE; LEFT_PARENS; bits_rhs = separated_list(COMMA, expression); RIGHT_PARENS; { { bits_lhs; bits_rhs; } }

let let_t :=
    | LET; lb_lhs = variables; EQUAL; lb_expr = expression; IN; lb_rhs = expression; { { lb_lhs; lb_expr; lb_rhs } }

let variables :=
    | v = annotated_variable; { [v] }
    | LEFT_ANGLE; vs = separated_list(COMMA, annotated_variable); RIGHT_ANGLE; { vs }

let case :=
    | CASE; case_exp = expression; OF; case_pat = annotated_clause+; END; { { case_exp; case_pat } }

let annotated_clause :=
    | c = clause; { c }
    | LEFT_PARENS; c = clause; DASH_PIPE; LEFT_BRACKET; _ = separated_list(COMMA, constant); RIGHT_BRACKET; RIGHT_PARENS; { c }

let clause :=
    | cp_lhs = patterns; cp_guard = guard; ARROW; cp_rhs = expression; { { cp_lhs; cp_guard; cp_rhs } }

let patterns :=
    | p = annotated_pattern; { [p] }
    | LEFT_ANGLE; ps = separated_list(COMMA, annotated_pattern); RIGHT_ANGLE; { ps }

let guard :=
    | WHEN; e = expression; { e }

let fun_t :=
    | FUN; LEFT_PARENS; fe_vars = separated_list(COMMA, annotated_variable); RIGHT_PARENS; ARROW; fe_body = expression; { { fe_vars; fe_body } }

let letrec :=
    | LETREC; lrb_lhs = function_definition*; IN; lrb_rhs = expression; { { lrb_lhs; lrb_rhs } }

let application :=
    | APPLY; fn_name = expression; LEFT_PARENS; fn_args = separated_list(COMMA, expression); RIGHT_PARENS; { (fn_name, fn_args) }

let inter_module_call :=
    | CALL; qc_mod = expression; COLON; qc_fun = expression; LEFT_PARENS; qc_args = separated_list(COMMA, expression); RIGHT_PARENS; { (qc_mod, qc_fun, qc_args) }

let prim_op_call :=
    | PRIMOP; pop_name = atom; LEFT_PARENS; pop_args = separated_list(COMMA, expression); RIGHT_PARENS; { (pop_name, pop_args) }

let try_t :=
    | TRY; tc_exp = expression; OF; tc_vars = variables; ARROW; tc_in = expression; CATCH; tc_catch_vars = variables; ARROW; tc_catch = expression; { { tc_exp; tc_vars; tc_in; tc_catch_vars; tc_catch } }

let receive :=
    | RECEIVE; rcv_pat = annotated_clause*; (tm_after, tm_body) = timeout; { { rcv_pat; tm_after; tm_body } }

let timeout :=
    | AFTER; tm_after = expression; ARROW; tm_body = expression; { (tm_after, tm_body) }

let sequencing :=
    | DO; e1 = expression; e2 = expression; { [e1; e2] }

let catch :=
    | CATCH; e = expression; { e }

let atom := (a, _) = ATOM; { a }
let integer := (i, _) = INTEGER; { i }
