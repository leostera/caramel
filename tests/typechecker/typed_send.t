  $ cat >typed_process.erl <<EOF
  > -module(typed_process).
  > -export([spawn_int/1, start/0]).
  > loop(Recv, S) ->
  >   receive
  >     {replace, X} -> loop(Recv, X);
  >     print -> _ = print_int(S), loop(Recv, S)
  >   end.
  > spawn_int(S) -> erlang:spawn(fun () -> loop(S) end).
  > start() -> spawn_int(0) ! {replace, yes}.
  > EOF
  $ caramelc compile --dump-ast erlang.ml typed_process.erl
  ((file_name typed_process.erl) (behaviours ()) (module_name typed_process)
    (ocaml_name Typed_process) (attributes ())
    (exports
      (((exp_type Export_function) (exp_name spawn_int) (exp_arity 1))
        ((exp_type Export_function) (exp_name start) (exp_arity 0))))
    (types ())
    (functions
      (((fd_name loop) (fd_arity 1)
         (fd_cases
           (((fc_name loop) (fc_lhs ((Pattern_binding S))) (fc_guards ())
              (fc_rhs
                (Expr_recv
                  ((rcv_cases
                     (((cb_pattern
                         (Pattern_tuple
                           ((Pattern_match (Lit_atom replace))
                             (Pattern_binding X))))
                        (cb_expr
                          (Expr_apply
                            ((fa_name (Expr_name (Atom_name loop)))
                              (fa_args ((Expr_name (Var_name X))))))))
                       ((cb_pattern (Pattern_match (Lit_atom print)))
                         (cb_expr
                           (Expr_let
                             ((lb_lhs (Pattern_binding _))
                               (lb_rhs
                                 (Expr_apply
                                   ((fa_name (Expr_name (Atom_name print_int)))
                                     (fa_args ((Expr_name (Var_name S))))))))
                             (Expr_apply
                               ((fa_name (Expr_name (Atom_name loop)))
                                 (fa_args ((Expr_name (Var_name S)))))))))))
                    (rcv_after ()))))))))
        ((fd_name spawn_int) (fd_arity 1)
          (fd_cases
            (((fc_name spawn_int) (fc_lhs ((Pattern_binding S))) (fc_guards ())
               (fc_rhs
                 (Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod erlang) (n_name spawn))))
                     (fa_args
                       ((Expr_fun
                          ((fd_name anonymous) (fd_arity 0)
                            (fd_cases
                              (((fc_name anonymous) (fc_lhs ()) (fc_guards ())
                                 (fc_rhs
                                   (Expr_apply
                                     ((fa_name (Expr_name (Atom_name loop)))
                                       (fa_args ((Expr_name (Var_name S)))))))))))))))))))))
        ((fd_name start) (fd_arity 0)
          (fd_cases
            (((fc_name start) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_apply
                   ((fa_name
                      (Expr_name (Qualified_name (n_mod erlang) (n_name send))))
                     (fa_args
                       ((Expr_apply
                          ((fa_name (Expr_name (Atom_name spawn_int)))
                            (fa_args ((Expr_literal (Lit_integer 0))))))
                         (Expr_tuple
                           ((Expr_literal (Lit_atom replace))
                             (Expr_literal (Lit_atom yes))))))))))))))))
