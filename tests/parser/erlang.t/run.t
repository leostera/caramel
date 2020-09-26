  $ caramelc compile --dump-ast *.erl
  ((file_name empty.erl) (behaviours ()) (module_name (Atom empty))
    (attributes ()) (exports ()) (types ()) (functions ()))
  
  module Empty = struct  end
  
  ((file_name function_declaration.erl) (behaviours ())
    (module_name (Atom function_declaration)) (attributes ()) (exports ())
    (types ())
    (functions
      (((fd_name (Atom literal_atom)) (fd_arity 0)
         (fd_cases
           (((c_lhs ()) (c_guard ())
              (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
         (fd_spec ()))
        ((fd_name (Atom literal_quoted_atom)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ())
               (c_rhs (Expr_literal (Lit_atom (Atom what.is_going:on!)))))))
          (fd_spec ()))
        ((fd_name (Atom literal_integer)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ()) (c_rhs (Expr_literal (Lit_integer 1))))))
          (fd_spec ()))
        ((fd_name (Atom literal_float)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ()) (c_rhs (Expr_literal (Lit_float 1.0))))))
          (fd_spec ()))
        ((fd_name (Atom tuple_empty)) (fd_arity 0)
          (fd_cases (((c_lhs ()) (c_guard ()) (c_rhs (Expr_tuple ())))))
          (fd_spec ()))
        ((fd_name (Atom tuple_nested)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ())
               (c_rhs
                 (Expr_tuple ((Expr_tuple ()) (Expr_tuple ((Expr_tuple ())))))))))
          (fd_spec ()))
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
          (fd_spec ()))
        ((fd_name (Atom list_empty)) (fd_arity 0)
          (fd_cases (((c_lhs ()) (c_guard ()) (c_rhs (Expr_list ())))))
          (fd_spec ()))
        ((fd_name (Atom list_nested)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ())
               (c_rhs
                 (Expr_list ((Expr_list ()) (Expr_list ((Expr_list ())))))))))
          (fd_spec ()))
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
          (fd_spec ()))
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
          (fd_spec ()))
        ((fd_name (Atom fun_args_atom)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pattern_match (Lit_atom (Atom ok))))) (c_guard ())
               (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
          (fd_spec ()))
        ((fd_name (Atom fun_args_quoted_atom)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pattern_match (Lit_atom (Atom what.is_going:on!)))))
               (c_guard ()) (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
          (fd_spec ()))
        ((fd_name (Atom fun_args_integer)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pattern_match (Lit_integer 1)))) (c_guard ())
               (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
          (fd_spec ()))
        ((fd_name (Atom fun_args_float)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pattern_match (Lit_float 1.0)))) (c_guard ())
               (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
          (fd_spec ()))
        ((fd_name (Atom fun_args_tuple_empty)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pattern_tuple ()))) (c_guard ())
               (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
          (fd_spec ()))
        ((fd_name (Atom fun_args_tuple_nested)) (fd_arity 1)
          (fd_cases
            (((c_lhs
                ((Pattern_tuple
                   ((Pattern_tuple ()) (Pattern_tuple ((Pattern_tuple ())))))))
               (c_guard ()) (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
          (fd_spec ()))
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
          (fd_spec ()))
        ((fd_name (Atom fun_args_list_empty)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pattern_list ()))) (c_guard ())
               (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
          (fd_spec ()))
        ((fd_name (Atom fun_args_list_nested)) (fd_arity 1)
          (fd_cases
            (((c_lhs
                ((Pattern_list
                   ((Pattern_list ()) (Pattern_list ((Pattern_list ())))))))
               (c_guard ()) (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
          (fd_spec ()))
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
          (fd_spec ()))
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
          (fd_spec ()))
        ((fd_name (Atom fun_arg_var_in_tuple)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pattern_tuple ((Pattern_binding (Var_name A))))))
               (c_guard ()) (c_rhs (Expr_name (Var_name A))))))
          (fd_spec ()))
        ((fd_name (Atom fun_arg_var_in_list)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pattern_list ((Pattern_binding (Var_name A))))))
               (c_guard ()) (c_rhs (Expr_name (Var_name A))))))
          (fd_spec ()))
        ((fd_name (Atom fun_arg_var)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
               (c_rhs (Expr_name (Var_name A))))))
          (fd_spec ()))
        ((fd_name (Atom fun_arg_var_ignore)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
               (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
          (fd_spec ()))
        ((fd_name (Atom fun_arg_var_ignore_in_tuple)) (fd_arity 1)
          (fd_cases
            (((c_lhs
                ((Pattern_tuple
                   ((Pattern_binding (Var_name A))
                     (Pattern_binding (Var_name _))))))
               (c_guard ()) (c_rhs (Expr_name (Var_name A))))))
          (fd_spec ()))
        ((fd_name (Atom fun_arg_var_ignore_in_list)) (fd_arity 1)
          (fd_cases
            (((c_lhs
                ((Pattern_list
                   ((Pattern_binding (Var_name _))
                     (Pattern_binding (Var_name B))))))
               (c_guard ()) (c_rhs (Expr_name (Var_name B))))))
          (fd_spec ()))
        ((fd_name (Atom fun_arg_var_ignore_in_cons)) (fd_arity 1)
          (fd_cases
            (((c_lhs
                ((Pattern_cons ((Pattern_binding (Var_name A)))
                   (Pattern_binding (Var_name _)))))
               (c_guard ()) (c_rhs (Expr_name (Var_name A))))))
          (fd_spec ()))
        ((fd_name (Atom fun_args)) (fd_arity 2)
          (fd_cases
            (((c_lhs
                ((Pattern_binding (Var_name A)) (Pattern_binding (Var_name B))))
               (c_guard ())
               (c_rhs
                 (Expr_tuple
                   ((Expr_name (Var_name A)) (Expr_name (Var_name B))))))))
          (fd_spec ()))
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
          (fd_spec ()))
        ((fd_name (Atom fun_args)) (fd_arity 4)
          (fd_cases
            (((c_lhs
                ((Pattern_binding (Var_name A)) (Pattern_binding (Var_name B))
                  (Pattern_binding (Var_name C))
                  (Pattern_binding (Var_name D))))
               (c_guard ())
               (c_rhs
                 (Expr_tuple
                   ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                     (Expr_name (Var_name C)) (Expr_name (Var_name D))))))))
          (fd_spec ()))
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
          (fd_spec ()))
        ((fd_name (Atom fun_args)) (fd_arity 6)
          (fd_cases
            (((c_lhs
                ((Pattern_binding (Var_name A)) (Pattern_binding (Var_name B))
                  (Pattern_binding (Var_name C)) (Pattern_binding (Var_name D))
                  (Pattern_binding (Var_name E))
                  (Pattern_binding (Var_name F))))
               (c_guard ())
               (c_rhs
                 (Expr_tuple
                   ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                     (Expr_name (Var_name C)) (Expr_name (Var_name D))
                     (Expr_name (Var_name E)) (Expr_name (Var_name F))))))))
          (fd_spec ()))
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
          (fd_spec ()))
        ((fd_name (Atom fun_args)) (fd_arity 8)
          (fd_cases
            (((c_lhs
                ((Pattern_binding (Var_name A)) (Pattern_binding (Var_name B))
                  (Pattern_binding (Var_name C)) (Pattern_binding (Var_name D))
                  (Pattern_binding (Var_name E)) (Pattern_binding (Var_name F))
                  (Pattern_binding (Var_name G))
                  (Pattern_binding (Var_name H))))
               (c_guard ())
               (c_rhs
                 (Expr_tuple
                   ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                     (Expr_name (Var_name C)) (Expr_name (Var_name D))
                     (Expr_name (Var_name E)) (Expr_name (Var_name F))
                     (Expr_name (Var_name G)) (Expr_name (Var_name H))))))))
          (fd_spec ()))
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
          (fd_spec ()))
        ((fd_name (Atom binding_return)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ())
               (c_rhs
                 (Expr_let
                   ((lb_lhs (Pattern_binding (Var_name A)))
                     (lb_rhs (Expr_literal (Lit_integer 1))))
                   (Expr_name (Var_name A)))))))
          (fd_spec ()))
        ((fd_name (Atom binding_and_return)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ())
               (c_rhs
                 (Expr_let
                   ((lb_lhs (Pattern_binding (Var_name A)))
                     (lb_rhs (Expr_literal (Lit_integer 1))))
                   (Expr_name (Var_name A)))))))
          (fd_spec ()))
        ((fd_name (Atom case_expr)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
               (c_rhs
                 (Expr_case (Expr_name (Var_name A))
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
                       (c_rhs (Expr_literal (Lit_atom (Atom true)))))))))))
          (fd_spec ()))
        ((fd_name (Atom fun_ref)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ())
               (c_rhs (Expr_fun_ref (fref_name (Atom fun_ref)) (fref_arity 0))))))
          (fd_spec ()))
        ((fd_name (Atom lambda)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ())
               (c_rhs
                 (Expr_fun
                   (((c_lhs ()) (c_guard ())
                      (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))))))
          (fd_spec ()))
        ((fd_name (Atom lambda_with_args)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ())
               (c_rhs
                 (Expr_fun
                   (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
                      (c_rhs (Expr_name (Var_name A))))))))))
          (fd_spec ()))
        ((fd_name (Atom lambda_in_var)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ())
               (c_rhs
                 (Expr_let
                   ((lb_lhs (Pattern_binding (Var_name F)))
                     (lb_rhs
                       (Expr_fun
                         (((c_lhs ((Pattern_binding (Var_name A))))
                            (c_guard ()) (c_rhs (Expr_name (Var_name A))))))))
                   (Expr_name (Var_name F)))))))
          (fd_spec ()))
        ((fd_name (Atom lambda_var_call)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ())
               (c_rhs
                 (Expr_let
                   ((lb_lhs (Pattern_binding (Var_name F)))
                     (lb_rhs
                       (Expr_fun
                         (((c_lhs ((Pattern_binding (Var_name A))))
                            (c_guard ()) (c_rhs (Expr_name (Var_name A))))))))
                   (Expr_apply
                     ((fa_name (Expr_name (Var_name F)))
                       (fa_args ((Expr_literal (Lit_integer 1)))))))))))
          (fd_spec ()))
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
          (fd_spec ()))
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
          (fd_spec ()))
        ((fd_name (Atom recv)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ())
               (c_rhs
                 (Expr_recv
                   ((rcv_cases
                      (((c_lhs ((Pattern_binding (Var_name X)))) (c_guard ())
                         (c_rhs (Expr_name (Var_name X))))))
                     (rcv_after ())))))))
          (fd_spec ()))
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
          (fd_spec ()))
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
          (fd_spec ()))
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
          (fd_spec ()))
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
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom +)))))
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
          (fd_spec ()))
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
                             ((fa_name
                                (Expr_name (Atom_name (Atom print_int))))
                               (fa_args ((Expr_name (Var_name A))))))))
                       (Expr_literal (Lit_atom (Atom ok))))))))))
          (fd_spec ())))))
  
  module Function_declaration =
    struct
      let rec literal_atom () = `ok
      let rec literal_quoted_atom () = `what.is_going:on!
      let rec literal_integer () = 1
      let rec literal_float () = 1.0
      let rec tuple_empty () = ()
      let rec tuple_nested () = ((), ())
      let rec tuple_filled () = `ok ((`error ("hello", 1)), (tuple_empty ()))
      let rec list_empty () = []
      let rec list_nested () = [[]; [[]]]
      let rec list_filled () = [`ok; [`error; "hello"; 1]; list_empty ()]
      let rec list_cons () = [`a; [`b `c; [list_empty (); []]]]
      let rec fun_args_atom `ok = `ok
      let rec fun_args_quoted_atom `what.is_going:on! = `ok
      let rec fun_args_integer 1 = `ok
      let rec fun_args_float 1.0 = `ok
      let rec fun_args_tuple_empty () = `ok
      let rec fun_args_tuple_nested ((), ()) = `ok
      let rec fun_args_tuple_filled (`ok (`error ("hello", 1))) = `ok
      let rec fun_args_list_empty [] = `ok
      let rec fun_args_list_nested ([]::([]::[])::[]) = `ok
      let rec fun_args_list_filled (`ok::(`error::"hello"::1::[])::[]) = `ok
      let rec fun_args_list_cons (`a::((`b `c)::(`f::[]::[])::[])::[]) = `ok
      let rec fun_arg_var_in_tuple a = a
      let rec fun_arg_var_in_list (a::[]) = a
      let rec fun_arg_var a = a
      let rec fun_arg_var_ignore _ = `ok
      let rec fun_arg_var_ignore_in_tuple (a, _) = a
      let rec fun_arg_var_ignore_in_list (_::b::[]) = b
      let rec fun_arg_var_ignore_in_cons (a::_::[]) = a
      let rec fun_args (a, b) = (a, b)
      let rec fun_args (a, b, c) = (a, b, c)
      let rec fun_args (a, b, c, d) = (a, b, c, d)
      let rec fun_args (a, b, c, d, e) = (a, b, c, d, e)
      let rec fun_args (a, b, c, d, e, f) = (a, b, c, d, e, f)
      let rec fun_args (a, b, c, d, e, f, g) = (a, b, c, d, e, f, g)
      let rec fun_args (a, b, c, d, e, f, g, h) = (a, b, c, d, e, f, g, h)
      let rec fun_args (a, b, c, d, e, f, g, h, i) =
        (a, b, c, d, e, f, g, h, i)
      let rec binding_return () = let a = 1 in a
      let rec binding_and_return () = let a = 1 in a
      let rec case_expr a =
        match a with
        | `true -> `false
        | `true -> `false
        | `false::[] -> `true
        | `false::_::[] -> `true
        | `false::b::t::[] -> `true
        | `false -> `true
      let rec fun_ref () = fun_ref
      let rec lambda () () = `ok
      let rec lambda_with_args () a = a
      let rec lambda_in_var () = let f a = a in f
      let rec lambda_var_call () = let f a = a in f 1
      let rec send a = Erlang.send (a, a)
      let rec send_chain a = Erlang.send (a, (Erlang.send (a, a)))
      let rec recv () = match recv () with | x -> x
      let rec recv_with_after () = match recv () with | x -> x
      let rec recv_selectively () =
        match recv () with
        | `true -> `false
        | `true -> `false
        | `false::[] -> `true
        | `false::_::[] -> `true
        | `false::b::t::[] -> `true
        | `false -> `true
      let rec fun_cases =
        function | 1 -> `ok | 2 -> `ok | 3 -> `ok | _ -> `false
      let rec fib =
        function
        | 0 -> 0
        | 1 -> 1
        | n ->
            Stdlib.(+) ((fib (Stdlib.(-) (n, 1))), (fib (Stdlib.(-) (n, 2))))
      let rec sequence () =
        let _ = print_string "hello" in
        let a = fib 2 in let _ = print_int a in `ok
    end
  
  File "_none_", line 1:
  Error: This expression has type 'a list
         but an expression was expected of type [> `ok ]
