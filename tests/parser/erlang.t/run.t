  $ caramelc compile --dump-ast *.erl
  ((file_name empty.erl) (behaviours ()) (module_name empty) (ocaml_name Empty)
    (attributes ()) (exports ()) (types ()) (functions ()))
  
  module Empty = struct  end
  
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
                          ((fa_name (Expr_name (Atom_name list_empty)))
                            (fa_args ()))))
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
               (fc_guards ()) (fc_rhs (Expr_name (Var_name B)))))))
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
               (fc_rhs (Expr_fun_ref fun_ref))))))
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
        ((fd_name send) (fd_arity 1)
          (fd_cases
            (((fc_name send) (fc_lhs ((Pattern_binding A))) (fc_guards ())
               (fc_rhs
                 (Expr_apply
                   ((fa_name
                      (Expr_name (Qualified_name (n_mod erlang) (n_name send))))
                     (fa_args
                       ((Expr_name (Var_name A)) (Expr_name (Var_name A)))))))))))
        ((fd_name send_chain) (fd_arity 1)
          (fd_cases
            (((fc_name send_chain) (fc_lhs ((Pattern_binding A)))
               (fc_guards ())
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
        ((fd_name recv_with_after) (fd_arity 0)
          (fd_cases
            (((fc_name recv_with_after) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_recv
                   ((rcv_cases
                      (((cb_pattern (Pattern_binding X))
                         (cb_expr (Expr_name (Var_name X))))))
                     (rcv_after
                       (((cb_pattern (Pattern_match (Lit_atom infinity)))
                          (cb_expr (Expr_literal (Lit_atom ok)))))))))))))
        ((fd_name recv_selectively) (fd_arity 0)
          (fd_cases
            (((fc_name recv_selectively) (fc_lhs ()) (fc_guards ())
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
                          (cb_expr (Expr_literal (Lit_atom ok)))))))))))))
        ((fd_name fun_cases) (fd_arity 1)
          (fd_cases
            (((fc_name fun_cases) (fc_lhs ((Pattern_match (Lit_integer 1))))
               (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok))))
              ((fc_name fun_cases) (fc_lhs ((Pattern_match (Lit_integer 2))))
                (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok))))
              ((fc_name fun_cases) (fc_lhs ((Pattern_match (Lit_integer 3))))
                (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom ok))))
              ((fc_name fun_cases) (fc_lhs ((Pattern_binding _)))
                (fc_guards ()) (fc_rhs (Expr_literal (Lit_atom false)))))))
        ((fd_name fib) (fd_arity 1)
          (fd_cases
            (((fc_name fib) (fc_lhs ((Pattern_match (Lit_integer 0))))
               (fc_guards ()) (fc_rhs (Expr_literal (Lit_integer 0))))
              ((fc_name fib) (fc_lhs ((Pattern_match (Lit_integer 1))))
                (fc_guards ()) (fc_rhs (Expr_literal (Lit_integer 1))))
              ((fc_name fib) (fc_lhs ((Pattern_binding N))) (fc_guards ())
                (fc_rhs
                  (Expr_apply
                    ((fa_name
                       (Expr_name (Qualified_name (n_mod erlang) (n_name +))))
                      (fa_args
                        ((Expr_apply
                           ((fa_name (Expr_name (Atom_name fib)))
                             (fa_args
                               ((Expr_apply
                                  ((fa_name
                                     (Expr_name
                                       (Qualified_name (n_mod erlang)
                                         (n_name -))))
                                    (fa_args
                                      ((Expr_name (Var_name N))
                                        (Expr_literal (Lit_integer 1))))))))))
                          (Expr_apply
                            ((fa_name (Expr_name (Atom_name fib)))
                              (fa_args
                                ((Expr_apply
                                   ((fa_name
                                      (Expr_name
                                        (Qualified_name (n_mod erlang)
                                          (n_name -))))
                                     (fa_args
                                       ((Expr_name (Var_name N))
                                         (Expr_literal (Lit_integer 2)))))))))))))))))))
        ((fd_name sequence) (fd_arity 0)
          (fd_cases
            (((fc_name sequence) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_let
                   ((lb_lhs (Pattern_binding _))
                     (lb_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name print_string)))
                           (fa_args ((Expr_literal (Lit_binary hello))))))))
                   (Expr_let
                     ((lb_lhs (Pattern_binding A))
                       (lb_rhs
                         (Expr_apply
                           ((fa_name (Expr_name (Atom_name fib)))
                             (fa_args ((Expr_literal (Lit_integer 2))))))))
                     (Expr_let
                       ((lb_lhs (Pattern_binding _))
                         (lb_rhs
                           (Expr_apply
                             ((fa_name (Expr_name (Atom_name print_int)))
                               (fa_args ((Expr_name (Var_name A))))))))
                       (Expr_literal (Lit_atom ok)))))))))))))
  
  module Function_declaration =
    struct
      let rec literal_atom () = `ok
      let rec literal_quoted_atom () = `What.is_going:on!
      let rec literal_integer () = 1
      let rec literal_float () = 1.0
      let rec tuple_empty () = ()
      let rec tuple_nested () = ((), ())
      let rec tuple_filled () = (`ok, (`error, "hello", 1), (tuple_empty ()))
      let rec list_empty () = []
      let rec list_nested () = [[]; [[]]]
      let rec list_filled () = [`ok; [`error; "hello"; 1]; list_empty ()]
      let rec list_cons () = [`a; [(`b, `c); [list_empty (); []]]]
      let rec fun_args_atom `ok = `ok
      let rec fun_args_quoted_atom `What.is_going:on! = `ok
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
      let rec send a = Stdlib.send a a
      let rec send_chain a = Stdlib.send a (Stdlib.send a a)
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
        | n -> Stdlib.(+) (fib (Stdlib.(-) n 1)) (fib (Stdlib.(-) n 2))
      let rec sequence () =
        let _ = print_string "hello" in
        let a = fib 2 in let _ = print_int a in `ok
    end
  
  File "_none_", line 1:
  Error: This expression has type 'a list
         but an expression was expected of type [> `ok ]
