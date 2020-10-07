  $ caramelc parse *.erl
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom comments)))))))
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom empty)))))))
  ((Function_decl
     ((fd_name (Atom sequence)) (fd_arity 0)
       (fd_cases
         (((c_lhs ()) (c_guard ())
            (c_rhs
              (Expr_let
                ((lb_lhs (Pattern_binding (Var_name _)))
                  (lb_rhs
                    (Expr_apply
                      ((fa_name (Expr_name (Atom_name (Atom print_string))))
                        (fa_args ((Expr_literal (Lit_binary hello))))))))
                (Expr_let
                  ((lb_lhs (Pattern_binding (Var_name A)))
                    (lb_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom fib))))
                          (fa_args ((Expr_literal (Lit_integer 2))))))))
                  (Expr_let
                    ((lb_lhs (Pattern_binding (Var_name _)))
                      (lb_rhs
                        (Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom print_int))))
                            (fa_args ((Expr_name (Var_name A))))))))
                    (Expr_literal (Lit_atom (Atom ok))))))))))
       (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fib)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_match (Lit_integer 0)))) (c_guard ())
             (c_rhs (Expr_literal (Lit_integer 0))))
            ((c_lhs ((Pattern_match (Lit_integer 1)))) (c_guard ())
              (c_rhs (Expr_literal (Lit_integer 1))))
            ((c_lhs ((Pattern_binding (Var_name N)))) (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom erlang)) (n_name (Atom +)))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom fib))))
                           (fa_args
                             ((Expr_apply
                                ((fa_name
                                   (Expr_name
                                     (Qualified_name (n_mod (Atom erlang))
                                       (n_name (Atom -)))))
                                  (fa_args
                                    ((Expr_name (Var_name N))
                                      (Expr_literal (Lit_integer 1))))))))))
                        (Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom fib))))
                            (fa_args
                              ((Expr_apply
                                 ((fa_name
                                    (Expr_name
                                      (Qualified_name (n_mod (Atom erlang))
                                        (n_name (Atom -)))))
                                   (fa_args
                                     ((Expr_name (Var_name N))
                                       (Expr_literal (Lit_integer 2))))))))))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_cases)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_match (Lit_integer 1)))) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))
            ((c_lhs ((Pattern_match (Lit_integer 2)))) (c_guard ())
              (c_rhs (Expr_literal (Lit_atom (Atom ok)))))
            ((c_lhs ((Pattern_match (Lit_integer 3)))) (c_guard ())
              (c_rhs (Expr_literal (Lit_atom (Atom ok)))))
            ((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
              (c_rhs (Expr_literal (Lit_atom (Atom false)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom recv_selectively)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_recv
                 ((rcv_cases
                    (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                       (c_guard ())
                       (c_rhs (Expr_literal (Lit_atom (Atom false)))))
                      ((c_lhs
                         ((Pattern_tuple
                            ((Pattern_match (Lit_atom (Atom true)))))))
                        (c_guard ())
                        (c_rhs (Expr_literal (Lit_atom (Atom false)))))
                      ((c_lhs
                         ((Pattern_list
                            ((Pattern_match (Lit_atom (Atom false)))))))
                        (c_guard ())
                        (c_rhs (Expr_literal (Lit_atom (Atom true)))))
                      ((c_lhs
                         ((Pattern_cons
                            ((Pattern_match (Lit_atom (Atom false))))
                            (Pattern_binding (Var_name _)))))
                        (c_guard ())
                        (c_rhs (Expr_literal (Lit_atom (Atom true)))))
                      ((c_lhs
                         ((Pattern_cons
                            ((Pattern_match (Lit_atom (Atom false)))
                              (Pattern_binding (Var_name B)))
                            (Pattern_binding (Var_name T)))))
                        (c_guard ())
                        (c_rhs (Expr_literal (Lit_atom (Atom true)))))
                      ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                        (c_guard ())
                        (c_rhs (Expr_literal (Lit_atom (Atom true)))))))
                   (rcv_after
                     (((c_lhs ((Pattern_match (Lit_atom (Atom infinity)))))
                        (c_guard ())
                        (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom recv_with_after)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_recv
                 ((rcv_cases
                    (((c_lhs ((Pattern_binding (Var_name X)))) (c_guard ())
                       (c_rhs (Expr_name (Var_name X))))))
                   (rcv_after
                     (((c_lhs ((Pattern_match (Lit_atom (Atom infinity)))))
                        (c_guard ())
                        (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom recv)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_recv
                 ((rcv_cases
                    (((c_lhs ((Pattern_binding (Var_name X)))) (c_guard ())
                       (c_rhs (Expr_name (Var_name X))))))
                   (rcv_after ())))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom send_chain)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom send)))))
                   (fa_args
                     ((Expr_name (Var_name A))
                       (Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom erlang))
                                (n_name (Atom send)))))
                           (fa_args
                             ((Expr_name (Var_name A))
                               (Expr_name (Var_name A))))))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom send)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom send)))))
                   (fa_args
                     ((Expr_name (Var_name A)) (Expr_name (Var_name A))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom lambda_var_call)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_let
                 ((lb_lhs (Pattern_binding (Var_name F)))
                   (lb_rhs
                     (Expr_fun
                       (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
                          (c_rhs (Expr_name (Var_name A))))))))
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name F)))
                     (fa_args ((Expr_literal (Lit_integer 1)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom lambda_in_var)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_let
                 ((lb_lhs (Pattern_binding (Var_name F)))
                   (lb_rhs
                     (Expr_fun
                       (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
                          (c_rhs (Expr_name (Var_name A))))))))
                 (Expr_name (Var_name F)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom lambda_with_args)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_fun
                 (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
                    (c_rhs (Expr_name (Var_name A))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom lambda)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_fun
                 (((c_lhs ()) (c_guard ())
                    (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_ref)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs (Expr_fun_ref (fref_name (Atom fun_ref)) (fref_arity 0))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom case_expr)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
             (c_rhs
               (Expr_case (Expr_name (Var_name A))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs (Expr_literal (Lit_atom (Atom false)))))
                   ((c_lhs
                      ((Pattern_tuple ((Pattern_match (Lit_atom (Atom true)))))))
                     (c_guard ())
                     (c_rhs (Expr_literal (Lit_atom (Atom false)))))
                   ((c_lhs
                      ((Pattern_list ((Pattern_match (Lit_atom (Atom false)))))))
                     (c_guard ())
                     (c_rhs (Expr_literal (Lit_atom (Atom true)))))
                   ((c_lhs
                      ((Pattern_cons ((Pattern_match (Lit_atom (Atom false))))
                         (Pattern_binding (Var_name _)))))
                     (c_guard ())
                     (c_rhs (Expr_literal (Lit_atom (Atom true)))))
                   ((c_lhs
                      ((Pattern_cons
                         ((Pattern_match (Lit_atom (Atom false)))
                           (Pattern_binding (Var_name B)))
                         (Pattern_binding (Var_name T)))))
                     (c_guard ())
                     (c_rhs (Expr_literal (Lit_atom (Atom true)))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs (Expr_literal (Lit_atom (Atom true)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom binding_and_return)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_let
                 ((lb_lhs (Pattern_binding (Var_name A)))
                   (lb_rhs (Expr_literal (Lit_integer 1))))
                 (Expr_name (Var_name A)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom binding_return)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_let
                 ((lb_lhs (Pattern_binding (Var_name A)))
                   (lb_rhs (Expr_literal (Lit_integer 1))))
                 (Expr_name (Var_name A)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args)) (fd_arity 9)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name A)) (Pattern_binding (Var_name B))
                (Pattern_binding (Var_name C)) (Pattern_binding (Var_name D))
                (Pattern_binding (Var_name E)) (Pattern_binding (Var_name F))
                (Pattern_binding (Var_name G)) (Pattern_binding (Var_name H))
                (Pattern_binding (Var_name I))))
             (c_guard ())
             (c_rhs
               (Expr_tuple
                 ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                   (Expr_name (Var_name C)) (Expr_name (Var_name D))
                   (Expr_name (Var_name E)) (Expr_name (Var_name F))
                   (Expr_name (Var_name G)) (Expr_name (Var_name H))
                   (Expr_name (Var_name I))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args)) (fd_arity 8)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name A)) (Pattern_binding (Var_name B))
                (Pattern_binding (Var_name C)) (Pattern_binding (Var_name D))
                (Pattern_binding (Var_name E)) (Pattern_binding (Var_name F))
                (Pattern_binding (Var_name G)) (Pattern_binding (Var_name H))))
             (c_guard ())
             (c_rhs
               (Expr_tuple
                 ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                   (Expr_name (Var_name C)) (Expr_name (Var_name D))
                   (Expr_name (Var_name E)) (Expr_name (Var_name F))
                   (Expr_name (Var_name G)) (Expr_name (Var_name H))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name A)) (Pattern_binding (Var_name B))
                (Pattern_binding (Var_name C)) (Pattern_binding (Var_name D))
                (Pattern_binding (Var_name E)) (Pattern_binding (Var_name F))
                (Pattern_binding (Var_name G))))
             (c_guard ())
             (c_rhs
               (Expr_tuple
                 ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                   (Expr_name (Var_name C)) (Expr_name (Var_name D))
                   (Expr_name (Var_name E)) (Expr_name (Var_name F))
                   (Expr_name (Var_name G))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name A)) (Pattern_binding (Var_name B))
                (Pattern_binding (Var_name C)) (Pattern_binding (Var_name D))
                (Pattern_binding (Var_name E)) (Pattern_binding (Var_name F))))
             (c_guard ())
             (c_rhs
               (Expr_tuple
                 ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                   (Expr_name (Var_name C)) (Expr_name (Var_name D))
                   (Expr_name (Var_name E)) (Expr_name (Var_name F))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name A)) (Pattern_binding (Var_name B))
                (Pattern_binding (Var_name C)) (Pattern_binding (Var_name D))
                (Pattern_binding (Var_name E))))
             (c_guard ())
             (c_rhs
               (Expr_tuple
                 ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                   (Expr_name (Var_name C)) (Expr_name (Var_name D))
                   (Expr_name (Var_name E))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name A)) (Pattern_binding (Var_name B))
                (Pattern_binding (Var_name C)) (Pattern_binding (Var_name D))))
             (c_guard ())
             (c_rhs
               (Expr_tuple
                 ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                   (Expr_name (Var_name C)) (Expr_name (Var_name D))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name A)) (Pattern_binding (Var_name B))
                (Pattern_binding (Var_name C))))
             (c_guard ())
             (c_rhs
               (Expr_tuple
                 ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                   (Expr_name (Var_name C))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name A)) (Pattern_binding (Var_name B))))
             (c_guard ())
             (c_rhs
               (Expr_tuple ((Expr_name (Var_name A)) (Expr_name (Var_name B))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_arg_var_ignore_in_cons)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name A)))
                 (Pattern_binding (Var_name _)))))
             (c_guard ()) (c_rhs (Expr_name (Var_name A))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_arg_var_ignore_in_list)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_list
                 ((Pattern_binding (Var_name _))
                   (Pattern_binding (Var_name B))))))
             (c_guard ()) (c_rhs (Expr_name (Var_name B))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_arg_var_ignore_in_tuple)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_tuple
                 ((Pattern_binding (Var_name A))
                   (Pattern_binding (Var_name _))))))
             (c_guard ()) (c_rhs (Expr_name (Var_name A))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_arg_var_ignore)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_arg_var)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
             (c_rhs (Expr_name (Var_name A))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_arg_var_in_list)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_list ((Pattern_binding (Var_name A))))))
             (c_guard ()) (c_rhs (Expr_name (Var_name A))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_arg_var_in_tuple)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_tuple ((Pattern_binding (Var_name A))))))
             (c_guard ()) (c_rhs (Expr_name (Var_name A))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_list_cons)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_match (Lit_atom (Atom a))))
                 (Pattern_cons
                   ((Pattern_tuple
                      ((Pattern_match (Lit_atom (Atom b)))
                        (Pattern_match (Lit_atom (Atom c))))))
                   (Pattern_cons ((Pattern_match (Lit_atom (Atom f))))
                     (Pattern_list ()))))))
             (c_guard ()) (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_list_filled)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_list
                 ((Pattern_match (Lit_atom (Atom ok)))
                   (Pattern_list
                     ((Pattern_match (Lit_atom (Atom error)))
                       (Pattern_match (Lit_binary hello))
                       (Pattern_match (Lit_integer 1))))))))
             (c_guard ()) (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_list_nested)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_list
                 ((Pattern_list ()) (Pattern_list ((Pattern_list ())))))))
             (c_guard ()) (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_list_empty)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_list ()))) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_tuple_filled)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_tuple
                 ((Pattern_match (Lit_atom (Atom ok)))
                   (Pattern_tuple
                     ((Pattern_match (Lit_atom (Atom error)))
                       (Pattern_match (Lit_binary hello))
                       (Pattern_match (Lit_integer 1))))))))
             (c_guard ()) (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_tuple_nested)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_tuple
                 ((Pattern_tuple ()) (Pattern_tuple ((Pattern_tuple ())))))))
             (c_guard ()) (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_tuple_empty)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_tuple ()))) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_float)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_match (Lit_float 1.0)))) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_integer)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_match (Lit_integer 1)))) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_quoted_atom)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_match (Lit_atom (Atom what.is_going:on!)))))
             (c_guard ()) (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_atom)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_match (Lit_atom (Atom ok))))) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom list_cons)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_cons ((Expr_literal (Lit_atom (Atom a))))
                 (Expr_cons
                   ((Expr_tuple
                      ((Expr_literal (Lit_atom (Atom b)))
                        (Expr_literal (Lit_atom (Atom c))))))
                   (Expr_cons
                     ((Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom list_empty))))
                          (fa_args ()))))
                     (Expr_list ()))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom list_filled)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_list
                 ((Expr_literal (Lit_atom (Atom ok)))
                   (Expr_list
                     ((Expr_literal (Lit_atom (Atom error)))
                       (Expr_literal (Lit_binary hello))
                       (Expr_literal (Lit_integer 1))))
                   (Expr_apply
                     ((fa_name (Expr_name (Atom_name (Atom list_empty))))
                       (fa_args ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom list_nested)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs (Expr_list ((Expr_list ()) (Expr_list ((Expr_list ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom list_empty)) (fd_arity 0)
        (fd_cases (((c_lhs ()) (c_guard ()) (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom tuple_filled)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_tuple
                 ((Expr_literal (Lit_atom (Atom ok)))
                   (Expr_tuple
                     ((Expr_literal (Lit_atom (Atom error)))
                       (Expr_literal (Lit_binary hello))
                       (Expr_literal (Lit_integer 1))))
                   (Expr_apply
                     ((fa_name (Expr_name (Atom_name (Atom tuple_empty))))
                       (fa_args ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom tuple_nested)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_tuple ((Expr_tuple ()) (Expr_tuple ((Expr_tuple ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom tuple_empty)) (fd_arity 0)
        (fd_cases (((c_lhs ()) (c_guard ()) (c_rhs (Expr_tuple ())))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom literal_float)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ()) (c_rhs (Expr_literal (Lit_float 1.0))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom literal_integer)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ()) (c_rhs (Expr_literal (Lit_integer 1))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom literal_quoted_atom)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom what.is_going:on!)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom literal_atom)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Module_attribute
      ((atr_name (Atom module))
        (atr_value (Expr_literal (Lit_atom (Atom function_declaration)))))))
  ((Module_attribute
     ((atr_name (Atom on_load))
       (atr_value
         (Expr_list
           ((Expr_tuple
              ((Expr_literal (Lit_atom (Atom pre)))
                (Expr_literal (Lit_integer 0)))))))))
    (Module_attribute
      ((atr_name (Atom export))
        (atr_value
          (Expr_list
            ((Expr_tuple
               ((Expr_literal (Lit_atom (Atom f)))
                 (Expr_literal (Lit_integer 0))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom g)))
                  (Expr_literal (Lit_integer 2)))))))))
    (Module_attribute
      ((atr_name (Atom export_type))
        (atr_value
          (Expr_list
            ((Expr_tuple
               ((Expr_literal (Lit_atom (Atom t)))
                 (Expr_literal (Lit_integer 0))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom opt)))
                  (Expr_literal (Lit_integer 2)))))))))
    (Module_attribute
      ((atr_name (Atom behaviour))
        (atr_value (Expr_literal (Lit_atom (Atom another_behavior))))))
    (Module_attribute
      ((atr_name (Atom behavior))
        (atr_value (Expr_literal (Lit_atom (Atom gen_server))))))
    (Module_attribute
      ((atr_name (Atom module))
        (atr_value (Expr_literal (Lit_atom (Atom module_attributes)))))))
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom type_declaration)))))))
  $ echo $?
  0
