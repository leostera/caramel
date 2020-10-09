  $ caramelc parse *.erl
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom 'My__path__to__module')))))))
  ((Module_comment (Comment " single comment"))
    (Module_attribute
      ((atr_name (Atom module))
        (atr_value (Expr_literal (Lit_atom (Atom comments))))))
    (Module_comment (Comment "%% multi \"%\""))
    (Module_attribute
      ((atr_name (Atom behavior))
        (atr_value (Expr_literal (Lit_atom (Atom b))))))
    (Module_comment (Comment "% many"))
    (Module_comment (Comment "% consecutive"))
    (Module_comment (Comment "% lines"))
    (Module_attribute
      ((atr_name (Atom author))
        (atr_value (Expr_literal (Lit_atom (Atom comment)))))))
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom constants))))))
    (Function_decl
      ((fd_name (Atom f)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_comment (Comment "% Integers")
                 (Expr_let
                   ((lb_lhs (Pattern_binding (Var_name _)))
                     (lb_rhs (Expr_literal (Lit_integer 1))))
                   (Expr_comment (Comment "% Floats")
                     (Expr_let
                       ((lb_lhs (Pattern_binding (Var_name _)))
                         (lb_rhs (Expr_literal (Lit_float 1.0))))
                       (Expr_comment (Comment "% Binaries")
                         (Expr_let
                           ((lb_lhs (Pattern_binding (Var_name _)))
                             (lb_rhs (Expr_literal (Lit_binary hello))))
                           (Expr_comment (Comment "% Strings")
                             (Expr_let
                               ((lb_lhs (Pattern_binding (Var_name _)))
                                 (lb_rhs (Expr_literal (Lit_string hello))))
                               (Expr_comment (Comment "% IO Lists")
                                 (Expr_let
                                   ((lb_lhs (Pattern_binding (Var_name _)))
                                     (lb_rhs
                                       (Expr_list
                                         ((Expr_literal (Lit_string hello))
                                           (Expr_list
                                             ((Expr_literal (Lit_string world))))))))
                                   (Expr_comment (Comment "% Chars")
                                     (Expr_let
                                       ((lb_lhs (Pattern_binding (Var_name _)))
                                         (lb_rhs
                                           (Expr_literal (Lit_atom (Atom c)))))
                                       (Expr_comment (Comment "% atoms")
                                         (Expr_let
                                           ((lb_lhs
                                              (Pattern_binding (Var_name _)))
                                             (lb_rhs
                                               (Expr_literal
                                                 (Lit_atom (Atom hello)))))
                                           (Expr_let
                                             ((lb_lhs
                                                (Pattern_binding (Var_name _)))
                                               (lb_rhs
                                                 (Expr_literal
                                                   (Lit_atom
                                                     (Atom 'Hello_!world.1')))))
                                             (Expr_name (Var_name _)))))))))))))))))))))
        (fd_spec ()))))
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom empty)))))))
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom expressions))))))
    (Function_decl
      ((fd_name (Atom f)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_comment (Comment "% Let bindings")
                 (Expr_let
                   ((lb_lhs (Pattern_binding (Var_name A)))
                     (lb_rhs (Expr_literal (Lit_integer 1))))
                   (Expr_let
                     ((lb_lhs (Pattern_binding (Var_name B)))
                       (lb_rhs (Expr_name (Var_name A))))
                     (Expr_let
                       ((lb_lhs (Pattern_binding (Var_name _)))
                         (lb_rhs (Expr_name (Var_name C))))
                       (Expr_comment (Comment "% Using variables")
                         (Expr_let
                           ((lb_lhs (Pattern_binding (Var_name _)))
                             (lb_rhs
                               (Expr_apply
                                 ((fa_name (Expr_name (Atom_name (Atom f))))
                                   (fa_args ((Expr_name (Var_name A))))))))
                           (Expr_comment (Comment "% Function references")
                             (Expr_let
                               ((lb_lhs (Pattern_binding (Var_name A)))
                                 (lb_rhs
                                   (Expr_fun_ref
                                     (fref_name (Atom_name (Atom f)))
                                     (fref_arity 0))))
                               (Expr_let
                                 ((lb_lhs (Pattern_binding (Var_name A)))
                                   (lb_rhs
                                     (Expr_fun_ref
                                       (fref_name
                                         (Qualified_name
                                           (n_mod
                                             (Atom_name (Atom expressions)))
                                           (n_name (Atom_name (Atom f)))))
                                       (fref_arity 0))))
                                 (Expr_let
                                   ((lb_lhs (Pattern_binding (Var_name A)))
                                     (lb_rhs
                                       (Expr_fun_ref
                                         (fref_name
                                           (Qualified_name (n_mod (Var_name A))
                                             (n_name (Atom_name (Atom f)))))
                                         (fref_arity 0))))
                                   (Expr_let
                                     ((lb_lhs (Pattern_binding (Var_name A)))
                                       (lb_rhs
                                         (Expr_fun_ref
                                           (fref_name
                                             (Qualified_name
                                               (n_mod (Var_name A))
                                               (n_name (Var_name B))))
                                           (fref_arity 0))))
                                     (Expr_let
                                       ((lb_lhs (Pattern_binding (Var_name A)))
                                         (lb_rhs
                                           (Expr_fun_ref
                                             (fref_name
                                               (Qualified_name
                                                 (n_mod
                                                   (Atom_name
                                                     (Atom expressions)))
                                                 (n_name (Var_name B))))
                                             (fref_arity 0))))
                                       (Expr_comment (Comment "% Lambdas")
                                         (Expr_let
                                           ((lb_lhs
                                              (Pattern_binding (Var_name A)))
                                             (lb_rhs
                                               (Expr_fun
                                                 (((c_lhs ()) (c_guard ())
                                                    (c_rhs
                                                      (Expr_literal
                                                        (Lit_atom (Atom ok)))))))))
                                           (Expr_let
                                             ((lb_lhs
                                                (Pattern_binding (Var_name A)))
                                               (lb_rhs
                                                 (Expr_fun
                                                   (((c_lhs
                                                       ((Pattern_binding
                                                          (Var_name B))))
                                                      (c_guard ())
                                                      (c_rhs
                                                        (Expr_name
                                                          (Var_name B))))))))
                                             (Expr_comment
                                               (Comment "% application")
                                               (Expr_let
                                                 ((lb_lhs
                                                    (Pattern_binding
                                                      (Var_name _)))
                                                   (lb_rhs
                                                     (Expr_apply
                                                       ((fa_name
                                                          (Expr_name
                                                            (Var_name A)))
                                                         (fa_args ())))))
                                                 (Expr_let
                                                   ((lb_lhs
                                                      (Pattern_binding
                                                        (Var_name _)))
                                                     (lb_rhs
                                                       (Expr_apply
                                                         ((fa_name
                                                            (Expr_name
                                                              (Atom_name
                                                                (Atom f))))
                                                           (fa_args ())))))
                                                   (Expr_let
                                                     ((lb_lhs
                                                        (Pattern_binding
                                                          (Var_name _)))
                                                       (lb_rhs
                                                         (Expr_apply
                                                           ((fa_name
                                                              (Expr_name
                                                                (Qualified_name
                                                                  (n_mod
                                                                    (Atom_name
                                                                      (Atom
                                                                      expressions)))
                                                                  (n_name
                                                                    (Atom_name
                                                                      (Atom f))))))
                                                             (fa_args ())))))
                                                     (Expr_let
                                                       ((lb_lhs
                                                          (Pattern_binding
                                                            (Var_name _)))
                                                         (lb_rhs
                                                           (Expr_apply
                                                             ((fa_name
                                                                (Expr_name
                                                                  (Qualified_name
                                                                    (n_mod
                                                                      (Var_name
                                                                      A))
                                                                    (n_name
                                                                      (Atom_name
                                                                      (Atom f))))))
                                                               (fa_args ())))))
                                                       (Expr_let
                                                         ((lb_lhs
                                                            (Pattern_binding
                                                              (Var_name _)))
                                                           (lb_rhs
                                                             (Expr_apply
                                                               ((fa_name
                                                                  (Expr_name
                                                                    (Qualified_name
                                                                      (n_mod
                                                                      (Var_name
                                                                      A))
                                                                      (n_name
                                                                      (Var_name
                                                                      B)))))
                                                                 (fa_args ())))))
                                                         (Expr_let
                                                           ((lb_lhs
                                                              (Pattern_binding
                                                                (Var_name _)))
                                                             (lb_rhs
                                                               (Expr_apply
                                                                 ((fa_name
                                                                    (Expr_name
                                                                      (Qualified_name
                                                                      (n_mod
                                                                      (Atom_name
                                                                      (Atom
                                                                      expressions)))
                                                                      (n_name
                                                                      (Var_name
                                                                      B)))))
                                                                   (fa_args ())))))
                                                           (Expr_comment
                                                             (Comment
                                                               "% Map expressions")
                                                             (Expr_let
                                                               ((lb_lhs
                                                                  (Pattern_binding
                                                                    (Var_name
                                                                      A)))
                                                                 (lb_rhs
                                                                   (Expr_map
                                                                     (((mf_name
                                                                      (Expr_literal
                                                                      (Lit_atom
                                                                      (Atom a))))
                                                                      (mf_value
                                                                      (Expr_literal
                                                                      (Lit_integer
                                                                      1))))
                                                                      ((mf_name
                                                                      (Expr_literal
                                                                      (Lit_atom
                                                                      (Atom b))))
                                                                      (mf_value
                                                                      (Expr_map
                                                                      (((mf_name
                                                                      (Expr_literal
                                                                      (Lit_atom
                                                                      (Atom c))))
                                                                      (mf_value
                                                                      (Expr_literal
                                                                      (Lit_atom
                                                                      (Atom d)))))))))))))
                                                               (Expr_let
                                                                 ((lb_lhs
                                                                    (Pattern_binding
                                                                      (Var_name
                                                                      B)))
                                                                   (lb_rhs
                                                                     (Expr_map_update
                                                                      (Expr_name
                                                                      (Var_name
                                                                      A))
                                                                      (((mf_name
                                                                      (Expr_literal
                                                                      (Lit_atom
                                                                      (Atom b))))
                                                                      (mf_value
                                                                      (Expr_literal
                                                                      (Lit_integer
                                                                      2))))))))
                                                                 (Expr_comment
                                                                   (Comment
                                                                     "% Record expressions")
                                                                   (Expr_comment
                                                                     (Comment
                                                                      " A = #record{ a=1 },")
                                                                     (Expr_comment
                                                                      (Comment
                                                                      " B = A#record{ a=1 },")
                                                                      (Expr_comment
                                                                      (Comment
                                                                      "% List expressions")
                                                                      (Expr_let
                                                                      ((lb_lhs
                                                                      (Pattern_binding
                                                                      (Var_name
                                                                      A)))
                                                                      (lb_rhs
                                                                      (Expr_list
                                                                      ())))
                                                                      (Expr_let
                                                                      ((lb_lhs
                                                                      (Pattern_binding
                                                                      (Var_name
                                                                      A)))
                                                                      (lb_rhs
                                                                      (Expr_list
                                                                      ((Expr_literal
                                                                      (Lit_integer
                                                                      1))
                                                                      (Expr_literal
                                                                      (Lit_integer
                                                                      2))
                                                                      (Expr_literal
                                                                      (Lit_integer
                                                                      3))))))
                                                                      (Expr_let
                                                                      ((lb_lhs
                                                                      (Pattern_binding
                                                                      (Var_name
                                                                      A)))
                                                                      (lb_rhs
                                                                      (Expr_cons
                                                                      ((Expr_literal
                                                                      (Lit_integer
                                                                      1)))
                                                                      (Expr_cons
                                                                      ((Expr_literal
                                                                      (Lit_integer
                                                                      2)))
                                                                      (Expr_cons
                                                                      ((Expr_literal
                                                                      (Lit_integer
                                                                      3)))
                                                                      (Expr_list
                                                                      ()))))))
                                                                      (Expr_comment
                                                                      (Comment
                                                                      "% Tuple expressions")
                                                                      (Expr_let
                                                                      ((lb_lhs
                                                                      (Pattern_binding
                                                                      (Var_name
                                                                      A)))
                                                                      (lb_rhs
                                                                      (Expr_tuple
                                                                      ())))
                                                                      (Expr_let
                                                                      ((lb_lhs
                                                                      (Pattern_binding
                                                                      (Var_name
                                                                      A)))
                                                                      (lb_rhs
                                                                      (Expr_tuple
                                                                      ((Expr_literal
                                                                      (Lit_integer
                                                                      1))))))
                                                                      (Expr_let
                                                                      ((lb_lhs
                                                                      (Pattern_binding
                                                                      (Var_name
                                                                      A)))
                                                                      (lb_rhs
                                                                      (Expr_tuple
                                                                      ((Expr_literal
                                                                      (Lit_integer
                                                                      1))
                                                                      (Expr_tuple
                                                                      ((Expr_literal
                                                                      (Lit_integer
                                                                      2))))))))
                                                                      (Expr_comment
                                                                      (Comment
                                                                      "% Try catch after")
                                                                      (Expr_let
                                                                      ((lb_lhs
                                                                      (Pattern_binding
                                                                      (Var_name
                                                                      _)))
                                                                      (lb_rhs
                                                                      (Expr_try
                                                                      ((try_expr
                                                                      (Expr_name
                                                                      (Var_name
                                                                      A)))
                                                                      (try_catch
                                                                      ((((c_lhs
                                                                      ((Pattern_catch
                                                                      ((Atom_name
                                                                      (Atom
                                                                      throw)))
                                                                      (Pattern_binding
                                                                      (Var_name
                                                                      Reason))
                                                                      ())))
                                                                      (c_guard
                                                                      ())
                                                                      (c_rhs
                                                                      (Expr_literal
                                                                      (Lit_atom
                                                                      (Atom ok)))))
                                                                      ((c_lhs
                                                                      ((Pattern_catch
                                                                      ((Var_name
                                                                      C))
                                                                      (Pattern_binding
                                                                      (Var_name
                                                                      R))
                                                                      ((Var_name
                                                                      S)))))
                                                                      (c_guard
                                                                      ())
                                                                      (c_rhs
                                                                      (Expr_literal
                                                                      (Lit_atom
                                                                      (Atom ok))))))))
                                                                      (try_after
                                                                      ((Expr_literal
                                                                      (Lit_atom
                                                                      (Atom ok)))))))))
                                                                      (Expr_comment
                                                                      (Comment
                                                                      "% Catch & throw")
                                                                      (Expr_let
                                                                      ((lb_lhs
                                                                      (Pattern_binding
                                                                      (Var_name
                                                                      A)))
                                                                      (lb_rhs
                                                                      (Expr_catch
                                                                      (Expr_name
                                                                      (Var_name
                                                                      B)))))
                                                                      (Expr_let
                                                                      ((lb_lhs
                                                                      (Pattern_binding
                                                                      (Var_name
                                                                      A)))
                                                                      (lb_rhs
                                                                      (Expr_apply
                                                                      ((fa_name
                                                                      (Expr_name
                                                                      (Qualified_name
                                                                      (n_mod
                                                                      (Atom_name
                                                                      (Atom
                                                                      erlang)))
                                                                      (n_name
                                                                      (Atom_name
                                                                      (Atom
                                                                      throw))))))
                                                                      (fa_args
                                                                      ((Expr_name
                                                                      (Var_name
                                                                      B))))))))
                                                                      (Expr_let
                                                                      ((lb_lhs
                                                                      (Pattern_binding
                                                                      (Var_name
                                                                      _)))
                                                                      (lb_rhs
                                                                      (Expr_case
                                                                      (Expr_name
                                                                      (Var_name
                                                                      A))
                                                                      (((c_lhs
                                                                      ((Pattern_binding
                                                                      (Var_name
                                                                      _))))
                                                                      (c_guard
                                                                      (((Expr_apply
                                                                      ((fa_name
                                                                      (Expr_name
                                                                      (Atom_name
                                                                      (Atom
                                                                      is_number))))
                                                                      (fa_args
                                                                      ((Expr_name
                                                                      (Var_name
                                                                      B)))))))))
                                                                      (c_rhs
                                                                      (Expr_literal
                                                                      (Lit_atom
                                                                      (Atom ok)))))))))
                                                                      (Expr_comment
                                                                      (Comment
                                                                      "% Receive expression")
                                                                      (Expr_let
                                                                      ((lb_lhs
                                                                      (Pattern_binding
                                                                      (Var_name
                                                                      _)))
                                                                      (lb_rhs
                                                                      (Expr_recv
                                                                      ((rcv_cases
                                                                      (((c_lhs
                                                                      ((Pattern_binding
                                                                      (Var_name
                                                                      _))))
                                                                      (c_guard
                                                                      ())
                                                                      (c_rhs
                                                                      (Expr_literal
                                                                      (Lit_atom
                                                                      (Atom ok)))))))
                                                                      (rcv_after
                                                                      (((c_lhs
                                                                      ((Pattern_match
                                                                      (Lit_integer
                                                                      1000))))
                                                                      (c_guard
                                                                      ())
                                                                      (c_rhs
                                                                      (Expr_literal
                                                                      (Lit_atom
                                                                      (Atom ok)))))))))))
                                                                      (Expr_literal
                                                                      (Lit_atom
                                                                      (Atom ok)))))))))))))))))))))))))))))))))))))))))))))))))))
        (fd_spec ()))))
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom function_declaration))))))
    (Function_decl
      ((fd_name (Atom literal_atom)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom literal_quoted_atom)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom 'What.is_going:on!')))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom literal_integer)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ()) (c_rhs (Expr_literal (Lit_integer 1))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom literal_float)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ()) (c_rhs (Expr_literal (Lit_float 1.0))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom tuple_empty)) (fd_arity 0)
        (fd_cases (((c_lhs ()) (c_guard ()) (c_rhs (Expr_tuple ())))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom tuple_nested)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_tuple ((Expr_tuple ()) (Expr_tuple ((Expr_tuple ())))))))))
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
      ((fd_name (Atom list_empty)) (fd_arity 0)
        (fd_cases (((c_lhs ()) (c_guard ()) (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom list_nested)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs (Expr_list ((Expr_list ()) (Expr_list ((Expr_list ())))))))))
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
      ((fd_name (Atom fun_args_atom)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_match (Lit_atom (Atom ok))))) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_quoted_atom)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_match (Lit_atom (Atom 'What.is_going:on!')))))
             (c_guard ()) (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_integer)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_match (Lit_integer 1)))) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_float)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_match (Lit_float 1.0)))) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_args_tuple_empty)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_tuple ()))) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
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
      ((fd_name (Atom fun_args_list_empty)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_list ()))) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
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
      ((fd_name (Atom fun_arg_var_in_tuple)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_tuple ((Pattern_binding (Var_name A))))))
             (c_guard ()) (c_rhs (Expr_name (Var_name A))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_arg_var_in_list)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_list ((Pattern_binding (Var_name A))))))
             (c_guard ()) (c_rhs (Expr_name (Var_name A))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_arg_var)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
             (c_rhs (Expr_name (Var_name A))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fun_arg_var_ignore)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
             (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
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
      ((fd_name (Atom fun_arg_var_ignore_in_list)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_list
                 ((Pattern_binding (Var_name _))
                   (Pattern_binding (Var_name B))))))
             (c_guard ()) (c_rhs (Expr_name (Var_name B))))))
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
      ((fd_name (Atom fun_args)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name A)) (Pattern_binding (Var_name B))))
             (c_guard ())
             (c_rhs
               (Expr_tuple ((Expr_name (Var_name A)) (Expr_name (Var_name B))))))))
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
      ((fd_name (Atom fun_ref)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_fun_ref (fref_name (Atom_name (Atom fun_ref)))
                 (fref_arity 0))))))
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
      ((fd_name (Atom lambda_with_args)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_fun
                 (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
                    (c_rhs (Expr_name (Var_name A))))))))))
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
      ((fd_name (Atom send)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom_name (Atom erlang)))
                        (n_name (Atom_name (Atom send))))))
                   (fa_args
                     ((Expr_name (Var_name A)) (Expr_name (Var_name A))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom send_chain)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name A)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom_name (Atom erlang)))
                        (n_name (Atom_name (Atom send))))))
                   (fa_args
                     ((Expr_name (Var_name A))
                       (Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom_name (Atom erlang)))
                                (n_name (Atom_name (Atom send))))))
                           (fa_args
                             ((Expr_name (Var_name A))
                               (Expr_name (Var_name A))))))))))))))
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
                       (Qualified_name (n_mod (Atom_name (Atom erlang)))
                         (n_name (Atom_name (Atom '+'))))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom fib))))
                           (fa_args
                             ((Expr_apply
                                ((fa_name
                                   (Expr_name
                                     (Qualified_name
                                       (n_mod (Atom_name (Atom erlang)))
                                       (n_name (Atom_name (Atom '-'))))))
                                  (fa_args
                                    ((Expr_name (Var_name N))
                                      (Expr_literal (Lit_integer 1))))))))))
                        (Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom fib))))
                            (fa_args
                              ((Expr_apply
                                 ((fa_name
                                    (Expr_name
                                      (Qualified_name
                                        (n_mod (Atom_name (Atom erlang)))
                                        (n_name (Atom_name (Atom '-'))))))
                                   (fa_args
                                     ((Expr_name (Var_name N))
                                       (Expr_literal (Lit_integer 2))))))))))))))))))
        (fd_spec ())))
    (Function_decl
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
        (fd_spec ()))))
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom macros))))))
    (Module_attribute
      ((atr_name (Atom define))
        (atr_value
          (Expr_list
            ((Expr_name (Var_name MY_MACRO)) (Expr_literal (Lit_integer 1)))))))
    (Module_attribute
      ((atr_name (Atom define))
        (atr_value
          (Expr_list
            ((Expr_apply
               ((fa_name (Expr_name (Var_name MY_MACRO)))
                 (fa_args ((Expr_name (Var_name X))))))
              (Expr_apply
                ((fa_name
                   (Expr_name
                     (Qualified_name (n_mod (Atom_name (Atom erlang)))
                       (n_name (Atom_name (Atom '+'))))))
                  (fa_args
                    ((Expr_name (Var_name X)) (Expr_literal (Lit_integer 1)))))))))))
    (Function_decl
      ((fd_name (Atom f)) (fd_arity 0)
        (fd_cases (((c_lhs ()) (c_guard ()) (c_rhs (Expr_macro ?MY_MACRO)))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom f)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_apply ((fa_name (Expr_macro ?MY_MACRO)) (fa_args ())))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom f)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_macro ?MY_MACRO))
                   (fa_args ((Expr_literal (Lit_integer 2))))))))))
        (fd_spec ()))))
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom module_attributes))))))
    (Module_attribute
      ((atr_name (Atom behavior))
        (atr_value (Expr_literal (Lit_atom (Atom gen_server))))))
    (Module_attribute
      ((atr_name (Atom behaviour))
        (atr_value (Expr_literal (Lit_atom (Atom another_behavior))))))
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
      ((atr_name (Atom on_load))
        (atr_value
          (Expr_list
            ((Expr_tuple
               ((Expr_literal (Lit_atom (Atom pre)))
                 (Expr_literal (Lit_integer 0))))))))))
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom operators))))))
    (Function_decl
      ((fd_name (Atom f)) (fd_arity 0)
        (fd_cases
          (((c_lhs ()) (c_guard ())
             (c_rhs
               (Expr_let
                 ((lb_lhs (Pattern_binding (Var_name _)))
                   (lb_rhs
                     (Expr_apply
                       ((fa_name
                          (Expr_name
                            (Qualified_name (n_mod (Atom_name (Atom erlang)))
                              (n_name (Atom_name (Atom '-'))))))
                         (fa_args ((Expr_literal (Lit_integer 1))))))))
                 (Expr_let
                   ((lb_lhs (Pattern_binding (Var_name _)))
                     (lb_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom_name (Atom erlang)))
                                (n_name (Atom_name (Atom send))))))
                           (fa_args
                             ((Expr_literal (Lit_integer 1))
                               (Expr_literal (Lit_integer 1))))))))
                   (Expr_let
                     ((lb_lhs (Pattern_binding (Var_name _)))
                       (lb_rhs
                         (Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name
                                  (n_mod (Atom_name (Atom erlang)))
                                  (n_name (Atom_name (Atom '+'))))))
                             (fa_args
                               ((Expr_literal (Lit_integer 1))
                                 (Expr_literal (Lit_integer 1))))))))
                     (Expr_let
                       ((lb_lhs (Pattern_binding (Var_name _)))
                         (lb_rhs
                           (Expr_apply
                             ((fa_name
                                (Expr_name
                                  (Qualified_name
                                    (n_mod (Atom_name (Atom erlang)))
                                    (n_name (Atom_name (Atom '-'))))))
                               (fa_args
                                 ((Expr_literal (Lit_integer 1))
                                   (Expr_literal (Lit_integer 1))))))))
                       (Expr_let
                         ((lb_lhs (Pattern_binding (Var_name _)))
                           (lb_rhs
                             (Expr_apply
                               ((fa_name
                                  (Expr_name
                                    (Qualified_name
                                      (n_mod (Atom_name (Atom erlang)))
                                      (n_name (Atom_name (Atom '/'))))))
                                 (fa_args
                                   ((Expr_literal (Lit_integer 1))
                                     (Expr_literal (Lit_integer 1))))))))
                         (Expr_let
                           ((lb_lhs (Pattern_binding (Var_name _)))
                             (lb_rhs
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name
                                      (Qualified_name
                                        (n_mod (Atom_name (Atom erlang)))
                                        (n_name (Atom_name (Atom '*'))))))
                                   (fa_args
                                     ((Expr_literal (Lit_integer 1))
                                       (Expr_literal (Lit_integer 1))))))))
                           (Expr_let
                             ((lb_lhs (Pattern_binding (Var_name _)))
                               (lb_rhs
                                 (Expr_apply
                                   ((fa_name
                                      (Expr_name
                                        (Qualified_name
                                          (n_mod (Atom_name (Atom erlang)))
                                          (n_name (Atom_name (Atom '++'))))))
                                     (fa_args
                                       ((Expr_literal (Lit_integer 1))
                                         (Expr_literal (Lit_integer 1))))))))
                             (Expr_let
                               ((lb_lhs (Pattern_binding (Var_name _)))
                                 (lb_rhs
                                   (Expr_apply
                                     ((fa_name
                                        (Expr_name
                                          (Qualified_name
                                            (n_mod (Atom_name (Atom erlang)))
                                            (n_name (Atom_name (Atom '--'))))))
                                       (fa_args
                                         ((Expr_literal (Lit_integer 1))
                                           (Expr_literal (Lit_integer 1))))))))
                               (Expr_let
                                 ((lb_lhs (Pattern_binding (Var_name _)))
                                   (lb_rhs
                                     (Expr_apply
                                       ((fa_name
                                          (Expr_name
                                            (Qualified_name
                                              (n_mod (Atom_name (Atom erlang)))
                                              (n_name (Atom_name (Atom '=='))))))
                                         (fa_args
                                           ((Expr_literal (Lit_integer 1))
                                             (Expr_literal (Lit_integer 1))))))))
                                 (Expr_let
                                   ((lb_lhs (Pattern_binding (Var_name _)))
                                     (lb_rhs
                                       (Expr_apply
                                         ((fa_name
                                            (Expr_name
                                              (Qualified_name
                                                (n_mod
                                                  (Atom_name (Atom erlang)))
                                                (n_name
                                                  (Atom_name (Atom '=:='))))))
                                           (fa_args
                                             ((Expr_literal (Lit_integer 1))
                                               (Expr_literal (Lit_integer 1))))))))
                                   (Expr_let
                                     ((lb_lhs (Pattern_binding (Var_name _)))
                                       (lb_rhs
                                         (Expr_apply
                                           ((fa_name
                                              (Expr_name
                                                (Qualified_name
                                                  (n_mod
                                                    (Atom_name (Atom erlang)))
                                                  (n_name
                                                    (Atom_name (Atom '=/='))))))
                                             (fa_args
                                               ((Expr_literal (Lit_integer 1))
                                                 (Expr_literal (Lit_integer 1))))))))
                                     (Expr_let
                                       ((lb_lhs (Pattern_binding (Var_name _)))
                                         (lb_rhs
                                           (Expr_apply
                                             ((fa_name
                                                (Expr_name
                                                  (Qualified_name
                                                    (n_mod
                                                      (Atom_name (Atom erlang)))
                                                    (n_name
                                                      (Atom_name (Atom '/='))))))
                                               (fa_args
                                                 ((Expr_literal
                                                    (Lit_integer 1))
                                                   (Expr_literal
                                                     (Lit_integer 1))))))))
                                       (Expr_let
                                         ((lb_lhs
                                            (Pattern_binding (Var_name _)))
                                           (lb_rhs
                                             (Expr_apply
                                               ((fa_name
                                                  (Expr_name
                                                    (Qualified_name
                                                      (n_mod
                                                        (Atom_name
                                                          (Atom erlang)))
                                                      (n_name
                                                        (Atom_name (Atom '<'))))))
                                                 (fa_args
                                                   ((Expr_literal
                                                      (Lit_integer 1))
                                                     (Expr_literal
                                                       (Lit_integer 1))))))))
                                         (Expr_let
                                           ((lb_lhs
                                              (Pattern_binding (Var_name _)))
                                             (lb_rhs
                                               (Expr_apply
                                                 ((fa_name
                                                    (Expr_name
                                                      (Qualified_name
                                                        (n_mod
                                                          (Atom_name
                                                            (Atom erlang)))
                                                        (n_name
                                                          (Atom_name
                                                            (Atom '=<'))))))
                                                   (fa_args
                                                     ((Expr_literal
                                                        (Lit_integer 1))
                                                       (Expr_literal
                                                         (Lit_integer 1))))))))
                                           (Expr_let
                                             ((lb_lhs
                                                (Pattern_binding (Var_name _)))
                                               (lb_rhs
                                                 (Expr_apply
                                                   ((fa_name
                                                      (Expr_name
                                                        (Qualified_name
                                                          (n_mod
                                                            (Atom_name
                                                              (Atom erlang)))
                                                          (n_name
                                                            (Atom_name
                                                              (Atom '>'))))))
                                                     (fa_args
                                                       ((Expr_literal
                                                          (Lit_integer 1))
                                                         (Expr_literal
                                                           (Lit_integer 1))))))))
                                             (Expr_let
                                               ((lb_lhs
                                                  (Pattern_binding
                                                    (Var_name _)))
                                                 (lb_rhs
                                                   (Expr_apply
                                                     ((fa_name
                                                        (Expr_name
                                                          (Qualified_name
                                                            (n_mod
                                                              (Atom_name
                                                                (Atom erlang)))
                                                            (n_name
                                                              (Atom_name
                                                                (Atom '>='))))))
                                                       (fa_args
                                                         ((Expr_literal
                                                            (Lit_integer 1))
                                                           (Expr_literal
                                                             (Lit_integer 1))))))))
                                               (Expr_literal
                                                 (Lit_atom (Atom ok)))))))))))))))))))))))
        (fd_spec ()))))
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom patterns))))))
    (Function_decl
      ((fd_name (Atom f)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_map
                 (((Pattern_match (Lit_atom (Atom x)))
                    (Pattern_binding (Var_name X)))))))
             (c_guard ()) (c_rhs (Expr_name (Var_name X))))))
        (fd_spec ()))))
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom type_declaration)))))))
  ((Module_attribute
     ((atr_name (Atom module))
       (atr_value (Expr_literal (Lit_atom (Atom types))))))
    (Type_decl
      ((typ_expr (Type_constr ((tc_name (Atom_name (Atom int))) (tc_args ()))))
        (typ_kind Type) (typ_name (Atom alias)) (typ_params ())))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom reference))) (tc_args ()))))
        (typ_kind Opaque) (typ_name (Atom opaque_alias)) (typ_params ())))
    (Type_decl
      ((typ_expr
         (Type_list
           (Type_constr ((tc_name (Atom_name (Atom alias))) (tc_args ())))))
        (typ_kind Type) (typ_name (Atom a_list)) (typ_params ())))
    (Type_decl
      ((typ_expr
         (Type_list
           (Type_constr ((tc_name (Atom_name (Atom alias))) (tc_args ())))))
        (typ_kind Type) (typ_name (Atom b_list)) (typ_params ())))
    (Type_decl
      ((typ_expr
         (Type_variant
           ((Type_const (Lit_atom (Atom a)))
             (Type_variant
               ((Type_const (Lit_atom (Atom b)))
                 (Type_const (Lit_atom (Atom c))))))))
        (typ_kind Type) (typ_name (Atom union)) (typ_params ())))
    (Type_decl
      ((typ_expr
         (Type_variant
           ((Type_const (Lit_atom (Atom a)))
             (Type_tuple
               ((Type_variant
                  ((Type_const (Lit_atom (Atom b)))
                    (Type_const (Lit_atom (Atom c))))))))))
        (typ_kind Type) (typ_name (Atom nested_union)) (typ_params ())))
    (Type_decl
      ((typ_expr (Type_map ())) (typ_kind Type) (typ_name (Atom empty_map))
        (typ_params ())))
    (Module_attribute
      ((atr_name (Atom record))
        (atr_value
          (Expr_list ((Expr_literal (Lit_atom (Atom r))) (Expr_tuple ()))))))
    (Type_decl
      ((typ_expr (Type_record ((Atom_name (Atom r)) ()))) (typ_kind Type)
        (typ_name (Atom record)) (typ_params ())))
    (Type_decl
      ((typ_expr (Type_const (Lit_atom (Atom unit)))) (typ_kind Spec)
        (typ_name (Atom fn)) (typ_params ())))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom union))) (tc_args ()))))
        (typ_kind Callback) (typ_name (Atom cb))
        (typ_params
          ((Type_constr ((tc_name (Atom_name (Atom a))) (tc_args ())))))))
    (Type_decl
      ((typ_expr
         (Type_variant
           ((Type_tuple
              ((Type_const (Lit_atom (Atom some)))
                (Type_variable (Var_name A))))
             (Type_const (Lit_atom (Atom none))))))
        (typ_kind Type) (typ_name (Atom option))
        (typ_params ((Type_variable (Var_name A)))))))
  $ echo $?
  0
