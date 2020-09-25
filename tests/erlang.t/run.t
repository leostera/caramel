  $ caramelc compile --dump-ast *.erl
  ((file_name empty.erl) (behaviours ()) (module_name empty) (ocaml_name Empty)
    (attributes ()) (exports ()) (types ()) (functions ()))
  ((file_name function_declaration.erl) (behaviours ())
    (module_name function_declaration) (ocaml_name Function_declaration)
    (attributes ()) (exports ()) (types ())
    (functions
      (((fd_name literal_atom) (fd_arity 0)
         (fd_cases
           (((fc_name literal_atom) (fc_lhs ()) (fc_guards ())
              (fc_rhs (Expr_literal (Lit_atom ok)))))))
        ((fd_name literal_quoted_atom) (fd_arity 0)
          (fd_cases
            (((fc_name literal_quoted_atom) (fc_lhs ()) (fc_guards ())
               (fc_rhs (Expr_literal (Lit_atom What.is_going:on!)))))))
        ((fd_name literal_integer) (fd_arity 0)
          (fd_cases
            (((fc_name literal_integer) (fc_lhs ()) (fc_guards ())
               (fc_rhs (Expr_literal (Lit_integer 1)))))))
        ((fd_name literal_float) (fd_arity 0)
          (fd_cases
            (((fc_name literal_float) (fc_lhs ()) (fc_guards ())
               (fc_rhs (Expr_literal (Lit_float 1.0)))))))
        ((fd_name tuple_empty) (fd_arity 0)
          (fd_cases
            (((fc_name tuple_empty) (fc_lhs ()) (fc_guards ())
               (fc_rhs (Expr_tuple ()))))))
        ((fd_name tuple_nested) (fd_arity 0)
          (fd_cases
            (((fc_name tuple_nested) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_tuple ((Expr_tuple ()) (Expr_tuple ((Expr_tuple ()))))))))))
        ((fd_name tuple_filled) (fd_arity 0)
          (fd_cases
            (((fc_name tuple_filled) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_tuple
                   ((Expr_literal (Lit_atom ok))
                     (Expr_tuple
                       ((Expr_literal (Lit_atom error))
                         (Expr_literal (Lit_binary hello))
                         (Expr_literal (Lit_integer 1))))
                     (Expr_apply
                       ((fa_name (Expr_name (Atom_name tuple_empty)))
                         (fa_args ()))))))))))
        ((fd_name list_empty) (fd_arity 0)
          (fd_cases
            (((fc_name list_empty) (fc_lhs ()) (fc_guards ())
               (fc_rhs (Expr_list ()))))))
        ((fd_name list_nested) (fd_arity 0)
          (fd_cases
            (((fc_name list_nested) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_list ((Expr_list ()) (Expr_list ((Expr_list ()))))))))))
        ((fd_name list_filled) (fd_arity 0)
          (fd_cases
            (((fc_name list_filled) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_list
                   ((Expr_literal (Lit_atom ok))
                     (Expr_list
                       ((Expr_literal (Lit_atom error))
                         (Expr_literal (Lit_binary hello))
                         (Expr_literal (Lit_integer 1))))
                     (Expr_apply
                       ((fa_name (Expr_name (Atom_name list_empty)))
                         (fa_args ()))))))))))
        ((fd_name list_cons) (fd_arity 0)
          (fd_cases
            (((fc_name list_cons) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_cons ((Expr_literal (Lit_atom a)))
                   (Expr_cons
                     ((Expr_tuple
                        ((Expr_literal (Lit_atom b))
                          (Expr_literal (Lit_atom c)))))
                     (Expr_cons
                       ((Expr_apply
                          ((fa_name (Expr_name (Atom_name f))) (fa_args ()))))
                       (Expr_list ())))))))))
        ((fd_name fun_args_atom) (fd_arity 1)
          (fd_cases
            (((fc_name fun_args_atom) (fc_lhs ((Pattern_match (Lit_atom ok))))
               (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok)))))))
        ((fd_name fun_args_quoted_atom) (fd_arity 1)
          (fd_cases
            (((fc_name fun_args_quoted_atom)
               (fc_lhs ((Pattern_match (Lit_atom What.is_going:on!))))
               (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok)))))))
        ((fd_name fun_args_integer) (fd_arity 1)
          (fd_cases
            (((fc_name fun_args_integer)
               (fc_lhs ((Pattern_match (Lit_integer 1)))) (fc_guards ())
               (fc_rhs (Expr_literal (Lit_atom ok)))))))
        ((fd_name fun_args_float) (fd_arity 1)
          (fd_cases
            (((fc_name fun_args_float)
               (fc_lhs ((Pattern_match (Lit_float 1.0)))) (fc_guards ())
               (fc_rhs (Expr_literal (Lit_atom ok)))))))
        ((fd_name fun_args_tuple_empty) (fd_arity 1)
          (fd_cases
            (((fc_name fun_args_tuple_empty) (fc_lhs ((Pattern_tuple ())))
               (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok)))))))
        ((fd_name fun_args_tuple_nested) (fd_arity 1)
          (fd_cases
            (((fc_name fun_args_tuple_nested)
               (fc_lhs
                 ((Pattern_tuple
                    ((Pattern_tuple ()) (Pattern_tuple ((Pattern_tuple ())))))))
               (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok)))))))
        ((fd_name fun_args_tuple_filled) (fd_arity 1)
          (fd_cases
            (((fc_name fun_args_tuple_filled)
               (fc_lhs
                 ((Pattern_tuple
                    ((Pattern_match (Lit_atom ok))
                      (Pattern_tuple
                        ((Pattern_match (Lit_atom error))
                          (Pattern_match (Lit_binary hello))
                          (Pattern_match (Lit_integer 1))))))))
               (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok)))))))
        ((fd_name fun_args_list_empty) (fd_arity 1)
          (fd_cases
            (((fc_name fun_args_list_empty) (fc_lhs ((Pattern_list ())))
               (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok)))))))
        ((fd_name fun_args_list_nested) (fd_arity 1)
          (fd_cases
            (((fc_name fun_args_list_nested)
               (fc_lhs
                 ((Pattern_list
                    ((Pattern_list ()) (Pattern_list ((Pattern_list ())))))))
               (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok)))))))
        ((fd_name fun_args_list_filled) (fd_arity 1)
          (fd_cases
            (((fc_name fun_args_list_filled)
               (fc_lhs
                 ((Pattern_list
                    ((Pattern_match (Lit_atom ok))
                      (Pattern_list
                        ((Pattern_match (Lit_atom error))
                          (Pattern_match (Lit_binary hello))
                          (Pattern_match (Lit_integer 1))))))))
               (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok)))))))
        ((fd_name fun_args_list_cons) (fd_arity 1)
          (fd_cases
            (((fc_name fun_args_list_cons)
               (fc_lhs
                 ((Pattern_cons ((Pattern_match (Lit_atom a)))
                    (Pattern_cons
                      ((Pattern_tuple
                         ((Pattern_match (Lit_atom b))
                           (Pattern_match (Lit_atom c)))))
                      (Pattern_cons ((Pattern_match (Lit_atom f)))
                        (Pattern_list ()))))))
               (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok)))))))
        ((fd_name fun_arg_var_in_tuple) (fd_arity 1)
          (fd_cases
            (((fc_name fun_arg_var_in_tuple)
               (fc_lhs ((Pattern_tuple ((Pattern_binding A))))) (fc_guards ())
               (fc_rhs (Expr_name (Var_name A)))))))
        ((fd_name fun_arg_var_in_list) (fd_arity 1)
          (fd_cases
            (((fc_name fun_arg_var_in_list)
               (fc_lhs ((Pattern_list ((Pattern_binding A))))) (fc_guards ())
               (fc_rhs (Expr_name (Var_name A)))))))
        ((fd_name fun_arg_var) (fd_arity 1)
          (fd_cases
            (((fc_name fun_arg_var) (fc_lhs ((Pattern_binding A)))
               (fc_guards ()) (fc_rhs (Expr_name (Var_name A)))))))
        ((fd_name fun_arg_var_ignore) (fd_arity 1)
          (fd_cases
            (((fc_name fun_arg_var_ignore) (fc_lhs ((Pattern_binding _)))
               (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok)))))))
        ((fd_name fun_arg_var_ignore_in_tuple) (fd_arity 1)
          (fd_cases
            (((fc_name fun_arg_var_ignore_in_tuple)
               (fc_lhs
                 ((Pattern_tuple ((Pattern_binding A) (Pattern_binding _)))))
               (fc_guards ()) (fc_rhs (Expr_name (Var_name A)))))))
        ((fd_name fun_arg_var_ignore_in_list) (fd_arity 1)
          (fd_cases
            (((fc_name fun_arg_var_ignore_in_list)
               (fc_lhs
                 ((Pattern_list ((Pattern_binding _) (Pattern_binding B)))))
               (fc_guards ()) (fc_rhs (Expr_name (Var_name A)))))))
        ((fd_name fun_arg_var_ignore_in_cons) (fd_arity 1)
          (fd_cases
            (((fc_name fun_arg_var_ignore_in_cons)
               (fc_lhs
                 ((Pattern_cons ((Pattern_binding A)) (Pattern_binding _))))
               (fc_guards ()) (fc_rhs (Expr_name (Var_name A)))))))
        ((fd_name fun_args) (fd_arity 2)
          (fd_cases
            (((fc_name fun_args)
               (fc_lhs ((Pattern_binding A) (Pattern_binding B)))
               (fc_guards ())
               (fc_rhs
                 (Expr_tuple
                   ((Expr_name (Var_name A)) (Expr_name (Var_name B)))))))))
        ((fd_name fun_args) (fd_arity 3)
          (fd_cases
            (((fc_name fun_args)
               (fc_lhs
                 ((Pattern_binding A) (Pattern_binding B) (Pattern_binding C)))
               (fc_guards ())
               (fc_rhs
                 (Expr_tuple
                   ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                     (Expr_name (Var_name C)))))))))
        ((fd_name fun_args) (fd_arity 4)
          (fd_cases
            (((fc_name fun_args)
               (fc_lhs
                 ((Pattern_binding A) (Pattern_binding B) (Pattern_binding C)
                   (Pattern_binding D)))
               (fc_guards ())
               (fc_rhs
                 (Expr_tuple
                   ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                     (Expr_name (Var_name C)) (Expr_name (Var_name D)))))))))
        ((fd_name fun_args) (fd_arity 5)
          (fd_cases
            (((fc_name fun_args)
               (fc_lhs
                 ((Pattern_binding A) (Pattern_binding B) (Pattern_binding C)
                   (Pattern_binding D) (Pattern_binding E)))
               (fc_guards ())
               (fc_rhs
                 (Expr_tuple
                   ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                     (Expr_name (Var_name C)) (Expr_name (Var_name D))
                     (Expr_name (Var_name E)))))))))
        ((fd_name fun_args) (fd_arity 6)
          (fd_cases
            (((fc_name fun_args)
               (fc_lhs
                 ((Pattern_binding A) (Pattern_binding B) (Pattern_binding C)
                   (Pattern_binding D) (Pattern_binding E) (Pattern_binding F)))
               (fc_guards ())
               (fc_rhs
                 (Expr_tuple
                   ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                     (Expr_name (Var_name C)) (Expr_name (Var_name D))
                     (Expr_name (Var_name E)) (Expr_name (Var_name F)))))))))
        ((fd_name fun_args) (fd_arity 7)
          (fd_cases
            (((fc_name fun_args)
               (fc_lhs
                 ((Pattern_binding A) (Pattern_binding B) (Pattern_binding C)
                   (Pattern_binding D) (Pattern_binding E) (Pattern_binding F)
                   (Pattern_binding G)))
               (fc_guards ())
               (fc_rhs
                 (Expr_tuple
                   ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                     (Expr_name (Var_name C)) (Expr_name (Var_name D))
                     (Expr_name (Var_name E)) (Expr_name (Var_name F))
                     (Expr_name (Var_name G)))))))))
        ((fd_name fun_args) (fd_arity 8)
          (fd_cases
            (((fc_name fun_args)
               (fc_lhs
                 ((Pattern_binding A) (Pattern_binding B) (Pattern_binding C)
                   (Pattern_binding D) (Pattern_binding E) (Pattern_binding F)
                   (Pattern_binding G) (Pattern_binding H)))
               (fc_guards ())
               (fc_rhs
                 (Expr_tuple
                   ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                     (Expr_name (Var_name C)) (Expr_name (Var_name D))
                     (Expr_name (Var_name E)) (Expr_name (Var_name F))
                     (Expr_name (Var_name G)) (Expr_name (Var_name H)))))))))
        ((fd_name fun_args) (fd_arity 9)
          (fd_cases
            (((fc_name fun_args)
               (fc_lhs
                 ((Pattern_binding A) (Pattern_binding B) (Pattern_binding C)
                   (Pattern_binding D) (Pattern_binding E) (Pattern_binding F)
                   (Pattern_binding G) (Pattern_binding H) (Pattern_binding I)))
               (fc_guards ())
               (fc_rhs
                 (Expr_tuple
                   ((Expr_name (Var_name A)) (Expr_name (Var_name B))
                     (Expr_name (Var_name C)) (Expr_name (Var_name D))
                     (Expr_name (Var_name E)) (Expr_name (Var_name F))
                     (Expr_name (Var_name G)) (Expr_name (Var_name H))
                     (Expr_name (Var_name I)))))))))
        ((fd_name binding_return) (fd_arity 0)
          (fd_cases
            (((fc_name binding_return) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_let
                   ((lb_lhs (Pattern_binding A))
                     (lb_rhs (Expr_literal (Lit_integer 1))))
                   (Expr_name (Var_name A))))))))
        ((fd_name binding_and_return) (fd_arity 0)
          (fd_cases
            (((fc_name binding_and_return) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_let
                   ((lb_lhs (Pattern_binding A))
                     (lb_rhs (Expr_literal (Lit_integer 1))))
                   (Expr_name (Var_name A))))))))
        ((fd_name case_expr) (fd_arity 1)
          (fd_cases
            (((fc_name case_expr) (fc_lhs ((Pattern_binding A))) (fc_guards ())
               (fc_rhs
                 (Expr_case (Expr_name (Var_name A))
                   (((cb_pattern (Pattern_match (Lit_atom true)))
                      (cb_expr (Expr_literal (Lit_atom false))))
                     ((cb_pattern
                        (Pattern_tuple ((Pattern_match (Lit_atom true)))))
                       (cb_expr (Expr_literal (Lit_atom false))))
                     ((cb_pattern
                        (Pattern_list ((Pattern_match (Lit_atom false)))))
                       (cb_expr (Expr_literal (Lit_atom true))))
                     ((cb_pattern
                        (Pattern_cons ((Pattern_match (Lit_atom false)))
                          (Pattern_binding _)))
                       (cb_expr (Expr_literal (Lit_atom true))))
                     ((cb_pattern
                        (Pattern_cons
                          ((Pattern_match (Lit_atom false))
                            (Pattern_binding B))
                          (Pattern_binding T)))
                       (cb_expr (Expr_literal (Lit_atom true))))
                     ((cb_pattern (Pattern_match (Lit_atom false)))
                       (cb_expr (Expr_literal (Lit_atom true)))))))))))
        ((fd_name fun_ref) (fd_arity 0)
          (fd_cases
            (((fc_name fun_ref) (fc_lhs ()) (fc_guards ())
               (fc_rhs (Expr_fun_ref f))))))
        ((fd_name lambda) (fd_arity 0)
          (fd_cases
            (((fc_name lambda) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_fun
                   ((fd_name anonymous) (fd_arity 0)
                     (fd_cases
                       (((fc_name anonymous) (fc_lhs ()) (fc_guards ())
                          (fc_rhs (Expr_literal (Lit_atom ok)))))))))))))
        ((fd_name lambda_with_args) (fd_arity 0)
          (fd_cases
            (((fc_name lambda_with_args) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_fun
                   ((fd_name anonymous) (fd_arity 1)
                     (fd_cases
                       (((fc_name anonymous) (fc_lhs ((Pattern_binding A)))
                          (fc_guards ()) (fc_rhs (Expr_name (Var_name A)))))))))))))
        ((fd_name lambda_in_var) (fd_arity 0)
          (fd_cases
            (((fc_name lambda_in_var) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_let
                   ((lb_lhs (Pattern_binding F))
                     (lb_rhs
                       (Expr_fun
                         ((fd_name anonymous) (fd_arity 1)
                           (fd_cases
                             (((fc_name anonymous)
                                (fc_lhs ((Pattern_binding A))) (fc_guards ())
                                (fc_rhs (Expr_name (Var_name A))))))))))
                   (Expr_name (Var_name F))))))))
        ((fd_name lambda_var_call) (fd_arity 0)
          (fd_cases
            (((fc_name lambda_var_call) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_let
                   ((lb_lhs (Pattern_binding F))
                     (lb_rhs
                       (Expr_fun
                         ((fd_name anonymous) (fd_arity 1)
                           (fd_cases
                             (((fc_name anonymous)
                                (fc_lhs ((Pattern_binding A))) (fc_guards ())
                                (fc_rhs (Expr_name (Var_name A))))))))))
                   (Expr_apply
                     ((fa_name (Expr_name (Var_name F)))
                       (fa_args ((Expr_literal (Lit_integer 1))))))))))))
        ((fd_name send) (fd_arity 0)
          (fd_cases
            (((fc_name send) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_apply
                   ((fa_name
                      (Expr_name (Qualified_name (n_mod erlang) (n_name send))))
                     (fa_args
                       ((Expr_name (Var_name A)) (Expr_name (Var_name A)))))))))))
        ((fd_name send) (fd_arity 0)
          (fd_cases
            (((fc_name send) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_apply
                   ((fa_name
                      (Expr_name (Qualified_name (n_mod erlang) (n_name send))))
                     (fa_args
                       ((Expr_name (Var_name A))
                         (Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod erlang) (n_name send))))
                             (fa_args
                               ((Expr_name (Var_name A))
                                 (Expr_name (Var_name A)))))))))))))))
        ((fd_name recv) (fd_arity 0)
          (fd_cases
            (((fc_name recv) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_recv
                   ((rcv_cases
                      (((cb_pattern (Pattern_binding X))
                         (cb_expr (Expr_name (Var_name X))))))
                     (rcv_after ()))))))))
        ((fd_name recv) (fd_arity 0)
          (fd_cases
            (((fc_name recv) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_recv
                   ((rcv_cases
                      (((cb_pattern (Pattern_binding X))
                         (cb_expr (Expr_name (Var_name X))))))
                     (rcv_after
                       (((cb_pattern (Pattern_match (Lit_atom infinity)))
                          (cb_expr (Expr_literal (Lit_atom ok)))))))))))))
        ((fd_name recv) (fd_arity 0)
          (fd_cases
            (((fc_name recv) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_recv
                   ((rcv_cases
                      (((cb_pattern (Pattern_match (Lit_atom true)))
                         (cb_expr (Expr_literal (Lit_atom false))))
                        ((cb_pattern
                           (Pattern_tuple ((Pattern_match (Lit_atom true)))))
                          (cb_expr (Expr_literal (Lit_atom false))))
                        ((cb_pattern
                           (Pattern_list ((Pattern_match (Lit_atom false)))))
                          (cb_expr (Expr_literal (Lit_atom true))))
                        ((cb_pattern
                           (Pattern_cons ((Pattern_match (Lit_atom false)))
                             (Pattern_binding _)))
                          (cb_expr (Expr_literal (Lit_atom true))))
                        ((cb_pattern
                           (Pattern_cons
                             ((Pattern_match (Lit_atom false))
                               (Pattern_binding B))
                             (Pattern_binding T)))
                          (cb_expr (Expr_literal (Lit_atom true))))
                        ((cb_pattern (Pattern_match (Lit_atom false)))
                          (cb_expr (Expr_literal (Lit_atom true))))))
                     (rcv_after
                       (((cb_pattern (Pattern_match (Lit_atom infinity)))
                          (cb_expr (Expr_literal (Lit_atom ok))))))))))))))))
  ((file_name module_attributes.erl) (behaviours (gen_server another_behavior))
    (module_name module_attributes) (ocaml_name Module_attributes)
    (attributes
      (((atr_name on_load)
         (atr_value
           (Expr_list
             ((Expr_tuple
                ((Expr_literal (Lit_atom pre)) (Expr_literal (Lit_integer 0))))))))))
    (exports
      (((exp_type Export_type) (exp_name t) (exp_arity 0))
        ((exp_type Export_type) (exp_name opt) (exp_arity 2))
        ((exp_type Export_function) (exp_name f) (exp_arity 0))
        ((exp_type Export_function) (exp_name g) (exp_arity 2))))
    (types ()) (functions ()))
  ((file_name type_declaration.erl) (behaviours ())
    (module_name type_declaration) (ocaml_name Type_declaration)
    (attributes ()) (exports ()) (types ()) (functions ()))
