  $ caramelc compile --dump-ast *.erl
  ((file_name empty.erl) (behaviours ()) (module_name empty) (ocaml_name Empty)
    (attributes ()) (exports ()) (types ()) (functions ()))
  ((file_name function_declaration.erl) (behaviours ())
    (module_name function_declaration) (ocaml_name Function_declaration)
    (attributes ()) (exports ()) (types ())
    (functions
      (((fd_name list_cons) (fd_arity 0)
         (fd_cases
           (((fc_name list_cons) (fc_lhs ()) (fc_guards ())
              (fc_rhs
                (Expr_list
                  ((Expr_literal (Lit_atom a))
                    (Expr_tuple
                      ((Expr_literal (Lit_atom b)) (Expr_literal (Lit_atom c))))
                    (Expr_apply
                      ((fa_name (Expr_name (Atom_name f))) (fa_args ()))))))))))
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
        ((fd_name list_nested) (fd_arity 0)
          (fd_cases
            (((fc_name list_nested) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_list ((Expr_list ()) (Expr_list ((Expr_list ()))))))))))
        ((fd_name list_empty) (fd_arity 0)
          (fd_cases
            (((fc_name list_empty) (fc_lhs ()) (fc_guards ())
               (fc_rhs (Expr_list ()))))))
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
        ((fd_name tuple_nested) (fd_arity 0)
          (fd_cases
            (((fc_name tuple_nested) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_tuple ((Expr_tuple ()) (Expr_tuple ((Expr_tuple ()))))))))))
        ((fd_name tuple_empty) (fd_arity 0)
          (fd_cases
            (((fc_name tuple_empty) (fc_lhs ()) (fc_guards ())
               (fc_rhs (Expr_tuple ()))))))
        ((fd_name literal_float) (fd_arity 0)
          (fd_cases
            (((fc_name literal_float) (fc_lhs ()) (fc_guards ())
               (fc_rhs (Expr_literal (Lit_float 1.0)))))))
        ((fd_name literal_integer) (fd_arity 0)
          (fd_cases
            (((fc_name literal_integer) (fc_lhs ()) (fc_guards ())
               (fc_rhs (Expr_literal (Lit_integer 1)))))))
        ((fd_name literal_atom) (fd_arity 0)
          (fd_cases
            (((fc_name literal_atom) (fc_lhs ()) (fc_guards ())
               (fc_rhs (Expr_literal (Lit_atom ok))))))))))
  ((file_name module_attributes.erl) (behaviours (another_behavior gen_server))
    (module_name module_attributes) (ocaml_name Module_attributes)
    (attributes
      (((atr_name on_load)
         (atr_value
           (Expr_list
             ((Expr_tuple
                ((Expr_literal (Lit_atom pre)) (Expr_literal (Lit_integer 0))))))))))
    (exports
      (((exp_type Export_function) (exp_name f) (exp_arity 0))
        ((exp_type Export_function) (exp_name g) (exp_arity 2))
        ((exp_type Export_type) (exp_name t) (exp_arity 0))
        ((exp_type Export_type) (exp_name opt) (exp_arity 2))))
    (types ()) (functions ()))
  ((file_name type_declaration.erl) (behaviours ())
    (module_name type_declaration) (ocaml_name Type_declaration)
    (attributes ()) (exports ()) (types ()) (functions ()))
