  $ cat main.caramel
  external format: string -> list<'a> -> unit = "io:format"
  
  pub fn main(args) {
    match args {
    | [] -> format("~s\n", [:bye_bye])
    | [x, ...rest] ->
        format("Hello, ~s!\n", [ x ]);
        main(rest)
    }
  }
  $ caramel parse --file main.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_extern
                      ((ext_name (Id (format)))
                        (ext_type
                          (Type_arrow (Type_name (Id (string)))
                            (Type_arrow (Type_apply (Id (list)) ((Type_var a)))
                              (Type_name (Id (unit))))))
                        (ext_symbol io:format) (ext_visibility Private)
                        (ext_annot ())))
                     (Str_fun
                       ((fn_visibility Public) (fn_name (Id (main)))
                         (fn_args ((No_label (Pat_bind (Id (args))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_match (Expr_var (Id (args)))
                             (((cs_lhs Pat_nil)
                                (cs_rhs
                                  (Expr_call (Expr_var (Id (format)))
                                    ((Expr_literal (Lit_string "~s\\n"))
                                      (Expr_cons
                                        (Expr_literal (Lit_atom bye_bye))
                                        Expr_nil)))))
                               ((cs_lhs
                                  (Pat_cons (Pat_bind (Id (x)))
                                    (Pat_bind (Id (rest)))))
                                 (cs_rhs
                                   (Expr_seq
                                     (Expr_call (Expr_var (Id (format)))
                                       ((Expr_literal
                                          (Lit_string "Hello, ~s!\\n"))
                                         (Expr_cons (Expr_var (Id (x)))
                                           Expr_nil)))
                                     (Expr_call (Expr_var (Id (main)))
                                       ((Expr_var (Id (rest)))))))))))
                         (fn_annot ()))))
