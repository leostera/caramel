open Core_ast
open Format

let pp_comma ppf () = pp_print_string ppf ","
let pp_comma_cut ppf () = pp_print_string ppf ","; pp_print_cut ppf ()
let pp_double_newline ppf () = pp_print_newline ppf (); pp_print_newline ppf ()

let pp_print_comma_list ppf = pp_print_list ~pp_sep:(pp_comma) ppf
let pp_print_cut_list ppf = pp_print_list ~pp_sep:(pp_print_cut) ppf
let pp_print_comma_cut_list ppf = pp_print_list ~pp_sep:(pp_comma_cut) ppf
let pp_print_double_newline_list ppf = pp_print_list ~pp_sep:(pp_double_newline) ppf

let pp_var ppf x = pp_print_string ppf x
let pp_atom ppf atom = fprintf ppf "'%s'" atom

let rec pp_literal ppf lit =
  match lit with
  | Lit_integer i -> pp_print_int ppf i
  | Lit_float f -> pp_print_float ppf f
  | Lit_char c -> fprintf ppf "$%c" c
  | Lit_string s -> fprintf ppf "\"%s\"" s
  | Lit_atom atom -> pp_atom ppf atom
  | Lit_nil -> pp_print_string ppf "[]"
  | Lit_cons (lhs, rhs) -> fprintf ppf "[%a|%a]"
    (pp_print_comma_list pp_literal) lhs
    pp_literal rhs
  | Lit_tuple lits -> fprintf ppf "{%a}"
    (pp_print_comma_list pp_literal) lits
  | Lit_list lits -> fprintf ppf "[%a]"
    (pp_print_comma_list pp_literal) lits

and pp_pattern ppf p =
  match p with
  | Pat_lit lit -> pp_literal ppf lit
  | Pat_var_name var -> pp_var ppf var
  | Pat_tuple ps -> fprintf ppf "{%a}"
    (pp_print_comma_list pp_pattern) ps
  | Pat_list ps -> fprintf ppf "[%a]"
    (pp_print_comma_list pp_pattern) ps
  | Pat_cons (lhs, rhs) -> fprintf ppf "[%a|%a]"
    (pp_print_comma_list pp_pattern) lhs
    pp_pattern rhs
  | Pat_bitstring ps -> fprintf ppf "#{%a}#"
    (pp_print_comma_list pp_bitstring_pattern) ps
  | Pat_alias (v, p) -> fprintf ppf "%a = %a"
    pp_var v
    pp_pattern p

and pp_expr ppf e =
  match e with
  | Expr_literal lit -> pp_literal ppf lit
  | Expr_var var -> pp_var ppf var
  | Expr_val_list vals -> fprintf ppf "<%a>"
    (pp_print_comma_list pp_expr) vals
  | Expr_fname fname -> pp_fname ppf fname
  | Expr_tuple es -> fprintf ppf "{%a}"
    (pp_print_comma_list pp_expr) es
  | Expr_list es -> fprintf ppf "[%a]"
    (pp_print_comma_list pp_expr) es
  | Expr_cons (lhs, rhs) -> fprintf ppf "[%a|%a]"
    (pp_print_comma_list pp_expr) lhs
    pp_expr rhs
  | Expr_binary bitstrings -> fprintf ppf "#{%a}#"
    (pp_print_comma_list pp_bitstring) bitstrings
  | Expr_let lb -> pp_let_binding ppf lb
  | Expr_letrec lrb -> pp_letrec_binding ppf lrb
  | Expr_case case -> pp_case_expr ppf case
  | Expr_apply { fn_name; fn_args } -> fprintf ppf "apply %a@,@[<v 4>    (%a)@]"
    pp_expr fn_name
    (pp_print_comma_list pp_expr) fn_args
  | Expr_qualified_call { qc_mod; qc_fun; qc_args } -> fprintf ppf "call %a:%a@,@[<v 4>    (%a)@]"
    pp_expr qc_mod
    pp_expr qc_fun
    (pp_print_comma_list pp_expr) qc_args
  | Expr_fun fe -> pp_fun_expr ppf fe
  | Expr_receive rcv -> pp_receive_expr ppf rcv
  | Expr_primop { pop_name; pop_args } -> fprintf ppf "primop %a@,@[<v 4>    (%a)@]"
    pp_atom pop_name
    (pp_print_comma_list pp_expr) pop_args
  | Expr_try tc -> pp_try_catch_expr ppf tc
  | Expr_do es -> fprintf ppf "do@ %a"
    (pp_print_cut_list pp_expr) es
  | Expr_catch e -> fprintf ppf "catch %a"
    pp_expr e

and pp_bitstring ppf { bits_lhs; bits_rhs } =
  fprintf ppf "#<%a>(%a)"
    pp_expr bits_lhs
    (pp_print_comma_list pp_expr) bits_rhs

and pp_bitstring_pattern ppf { bits_pat_lhs; bits_pat_rhs } =
  fprintf ppf "#<%a>(%a)"
    pp_pattern bits_pat_lhs
    (pp_print_comma_list pp_expr) bits_pat_rhs

and pp_let_binding ppf { lb_lhs; lb_expr; lb_rhs } =
  fprintf ppf "let <%a> =@,@[<v 4>    %a@]@ @[<hov>in@ %a@]"
    (pp_print_comma_list pp_var) lb_lhs
    pp_expr lb_expr
    pp_expr lb_rhs

and pp_letrec_binding ppf { lrb_lhs; lrb_rhs } =
  fprintf ppf "letrec @,@[<v 4>    %a@]@ @[<hov>in@ %a@]"
    (pp_print_cut_list pp_fun_def) lrb_lhs
    pp_expr lrb_rhs

and pp_case_expr ppf { case_exp; case_pat } =
  fprintf ppf "case %a of@ @[<v 4>    %a@]@ end"
    pp_expr case_exp
    (pp_print_cut_list pp_clause) case_pat

and pp_clause ppf { cp_lhs; cp_guard; cp_rhs } =
  fprintf ppf "<%a> when %a ->@,@[<v 4>    %a@]"
    (pp_print_comma_list pp_pattern) cp_lhs
    pp_expr cp_guard
    pp_expr cp_rhs

and pp_fun_expr ppf { fe_vars; fe_body } =
  fprintf ppf "fun (%a) ->@,%a"
    (pp_print_comma_list pp_var) fe_vars
    pp_expr fe_body

and pp_receive_expr ppf { rcv_pat; tm_after; tm_body } =
  fprintf ppf "receive %a@ after %a ->@,@[<v 4>    %a@]"
    (pp_print_cut_list pp_clause) rcv_pat
    pp_expr tm_after
    pp_expr tm_body

and pp_try_catch_expr ppf { tc_exp; tc_vars; tc_in; tc_catch_vars; tc_catch } =
  fprintf ppf "try %a of <%a> ->@,@[<v 4>    %a@]@ catch <%a> ->@,@[<v 4>    %a@]"
    pp_expr tc_exp
    (pp_print_comma_list pp_var) tc_vars
    pp_expr tc_in
    (pp_print_comma_list pp_var) tc_catch_vars
    pp_expr tc_catch

and pp_fname ppf { fn_name; fn_arity } =
  fprintf ppf "%a/%a"
    pp_atom fn_name
    pp_print_int fn_arity

and pp_fun_def ppf { fd_name; fd_body } =
  fprintf ppf "%a =@,@[<v 4>    %a@]"
    pp_fname fd_name
    pp_fun_expr fd_body

let pp_attribute ppf { atr_name; atr_value } =
  fprintf ppf "%a =@,@[<v 4>    %a@]"
    pp_atom atr_name
    pp_literal atr_value

let pp ?(module_info = false) ppf { m_name; m_fnames; m_attributes; m_defs; _ } =
  let s1 = if module_info then "\n'module_info'/0,\n'module_info'/1" else "" in
  let s2 = if module_info then {|
'module_info'/0 =
    ( fun () ->
    call 'erlang':'get_module_info'
        ('expected')
      -| [{'function',{'module_info',0; }; }] )

'module_info'/1 =
    ( fun (_0) ->
    call 'erlang':'get_module_info'
        ('expected', ( _0
           -| [{'function',{'module_info',1; }; }] ))
      -| [{'function',{'module_info',1; }; }] )|} else "" in
  fprintf ppf "module %a [@[<v>%a@]%s]@.attributes [@[<v>%a@]]@.@.%a%s@.@.end"
    pp_atom m_name
    (pp_print_comma_cut_list pp_fname) m_fnames
    s1
    (pp_print_comma_cut_list pp_attribute) m_attributes
    (pp_print_double_newline_list pp_fun_def) m_defs
    s2

let to_source_file coremod =
  let _ = print_string ("Compiling " ^ coremod.m_filename ^ "\t") in
  let corefile = coremod.m_filename in
  let oc = open_out_bin corefile in
  (try
     let f = formatter_of_out_channel oc in
     fprintf f "%% Source code generated with Caramel.\n";
     fprintf f "%a@\n%!" (pp ~module_info:true) coremod
   with _ -> Sys.remove corefile);
  print_string "OK\n";
  close_out oc

let to_sources coremod = List.iter to_source_file coremod
