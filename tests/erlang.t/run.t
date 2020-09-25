  $ caramelc compile --dump-ast *.erl
  ((file_name empty.erl) (behaviours ()) (module_name empty) (ocaml_name Empty)
    (attributes ()) (exports ()) (types ()) (functions ()))
  ((file_name function_declaration.erl) (behaviours ())
    (module_name function_declaration) (ocaml_name Function_declaration)
    (attributes ()) (exports ()) (types ())
    (functions
      (((fd_name literal_list) (fd_arity 0)
         (fd_cases
           (((fc_name literal_list) (fc_lhs ()) (fc_guards ())
              (fc_rhs (Expr_list ()))))))
        ((fd_name literal_tuple) (fd_arity 0)
          (fd_cases
            (((fc_name literal_tuple) (fc_lhs ()) (fc_guards ())
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
