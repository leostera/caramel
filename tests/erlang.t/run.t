  $ caramelc compile --dump-ast *.erl
  ((file_name empty.erl) (behaviours ()) (module_name empty) (ocaml_name Empty)
    (attributes ()) (exports ()) (types ()) (functions ()))
  ((file_name function_declaration.erl) (behaviours ())
    (module_name function_declaration) (ocaml_name Function_declaration)
    (attributes ()) (exports ()) (types ()) (functions ()))
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
