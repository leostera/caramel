  $ cat rogue_one.erl
  -module(rogue_one).
  
  -behavior(gen_server).
  -behavior(test_beh).
  
  -export_type([t/0]).
  -export([f/0]).
  
  f() -> print_int(<<"hello">>).
  
  g(1) -> ok;
  g(0) -> rogue_one:f(err).
  $ caramelc compile --dump-ast rogue_one.erl
  ((file_name rogue_one.erl) (behaviours (test_beh gen_server))
    (module_name rogue_one) (ocaml_name Rogue_one) (attributes ())
    (exports
      (((exp_type Export_function) (exp_name f) (exp_arity 0))
        ((exp_type Export_type) (exp_name t) (exp_arity 0))))
    (types ())
    (functions
      (((fd_name g) (fd_arity 1)
         (fd_cases
           (((fc_name g) (fc_lhs ((Pattern_match (Lit_integer 1))))
              (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok))))
             ((fc_name g) (fc_lhs ((Pattern_match (Lit_integer 0))))
               (fc_guards ())
               (fc_rhs
                 (Expr_apply
                   ((fa_name
                      (Expr_name (Qualified_name (n_mod rogue_one) (n_name f))))
                     (fa_args ((Expr_literal (Lit_atom err)))))))))))
        ((fd_name f) (fd_arity 0)
          (fd_cases
            (((fc_name f) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name print_int)))
                     (fa_args ((Expr_literal (Lit_binary hello))))))))))))))
