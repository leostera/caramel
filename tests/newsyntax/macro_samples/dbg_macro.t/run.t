  $ cat main.caramel
  external format: string -> list<'a> -> unit = "io:format"
  
  pub macro println(val) {
    quote {
      format("~p\n", [val])
    }
  }
  
  pub macro dbg(msg, val) {
      println( (unquote{msg}, unquote{val}) )
  }
  
  pub fn main(args) {
    dbg("input:", (:args, args))
  }
  $ caramel parse --file main.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] eval: (Expr_call
                           (Expr_lambda
                             ((No_label (Pat_bind (Id (msg))))
                               (No_label (Pat_bind (Id (val)))))
                             (Expr_call (Expr_var (Id (println)))
                               ((Expr_tuple
                                  ((Expr_unquote (Expr_var (Id (msg))))
                                    (Expr_unquote (Expr_var (Id (val)))))))))
                           ((Expr_literal (Lit_string input:))
                             (Expr_tuple
                               ((Expr_literal (Lit_atom args))
                                 (Expr_var (Id (args)))))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_string input:))
  caramel: [DEBUG] eval: (Expr_tuple
                           ((Expr_literal (Lit_atom args))
                             (Expr_var (Id (args)))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom args))
  caramel: [DEBUG] eval: (Expr_var (Id (args)))
  caramel: [DEBUG] eval: (Expr_call (Expr_var (Id (println)))
                           ((Expr_tuple
                              ((Expr_literal (Lit_string input:))
                                (Expr_tuple
                                  ((Expr_literal (Lit_atom args))
                                    (Expr_var (Id (args)))))))))
  caramel: [DEBUG] eval: (Expr_tuple
                           ((Expr_literal (Lit_string input:))
                             (Expr_tuple
                               ((Expr_literal (Lit_atom args))
                                 (Expr_var (Id (args)))))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_string input:))
  caramel: [DEBUG] eval: (Expr_tuple
                           ((Expr_literal (Lit_atom args))
                             (Expr_var (Id (args)))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom args))
  caramel: [DEBUG] eval: (Expr_var (Id (args)))
  caramel: [DEBUG] calling: (Expr_var (Id (println)))
  caramel: [DEBUG] eval: (Expr_call
                           (Expr_lambda ((No_label (Pat_bind (Id (val)))))
                             (Expr_quote
                               (Expr_call (Expr_var (Id (format)))
                                 ((Expr_literal (Lit_string "~p\\n"))
                                   (Expr_cons (Expr_var (Id (val))) Expr_nil)))))
                           ((Expr_tuple
                              ((Expr_literal (Lit_string input:))
                                (Expr_tuple
                                  ((Expr_literal (Lit_atom args))
                                    (Expr_var (Id (args)))))))))
  caramel: [DEBUG] eval: (Expr_tuple
                           ((Expr_literal (Lit_string input:))
                             (Expr_tuple
                               ((Expr_literal (Lit_atom args))
                                 (Expr_var (Id (args)))))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_string input:))
  caramel: [DEBUG] eval: (Expr_tuple
                           ((Expr_literal (Lit_atom args))
                             (Expr_var (Id (args)))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom args))
  caramel: [DEBUG] eval: (Expr_var (Id (args)))
  caramel: [DEBUG] eval: (Expr_quote
                           (Expr_call (Expr_var (Id (format)))
                             ((Expr_literal (Lit_string "~p\\n"))
                               (Expr_cons
                                 (Expr_tuple
                                   ((Expr_literal (Lit_string input:))
                                     (Expr_tuple
                                       ((Expr_literal (Lit_atom args))
                                         (Expr_var (Id (args)))))))
                                 Expr_nil))))
  caramel: [DEBUG] eval: (Expr_call (Expr_var (Id (format)))
                           ((Expr_literal (Lit_string "~p\\n"))
                             (Expr_cons
                               (Expr_tuple
                                 ((Expr_literal (Lit_string input:))
                                   (Expr_tuple
                                     ((Expr_literal (Lit_atom args))
                                       (Expr_var (Id (args)))))))
                               Expr_nil)))
  caramel: [DEBUG] eval: (Expr_literal (Lit_string "~p\\n"))
  caramel: [DEBUG] eval: (Expr_cons
                           (Expr_tuple
                             ((Expr_literal (Lit_string input:))
                               (Expr_tuple
                                 ((Expr_literal (Lit_atom args))
                                   (Expr_var (Id (args)))))))
                           Expr_nil)
  caramel: [DEBUG] eval: Expr_nil
  caramel: [DEBUG] eval: (Expr_tuple
                           ((Expr_literal (Lit_string input:))
                             (Expr_tuple
                               ((Expr_literal (Lit_atom args))
                                 (Expr_var (Id (args)))))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_string input:))
  caramel: [DEBUG] eval: (Expr_tuple
                           ((Expr_literal (Lit_atom args))
                             (Expr_var (Id (args)))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom args))
  caramel: [DEBUG] eval: (Expr_var (Id (args)))
  caramel: [DEBUG] eval: (Expr_var (Id (format)))
  caramel: [DEBUG] Expanded program: ((Str_extern
                                        ((ext_name (Id (format)))
                                          (ext_type
                                            (Type_arrow
                                              (Type_name (Id (string)))
                                              (Type_arrow
                                                (Type_apply (Id (list))
                                                  ((Type_var a)))
                                                (Type_name (Id (unit))))))
                                          (ext_symbol io:format)
                                          (ext_visibility Private)
                                          (ext_annot ())))
                                       (Str_macro
                                         ((fn_visibility Public)
                                           (fn_name (Id (println)))
                                           (fn_args
                                             ((No_label (Pat_bind (Id (val))))))
                                           (fn_arity 1)
                                           (fn_body
                                             (Expr_quote
                                               (Expr_call
                                                 (Expr_var (Id (format)))
                                                 ((Expr_literal
                                                    (Lit_string "~p\\n"))
                                                   (Expr_cons
                                                     (Expr_var (Id (val)))
                                                     Expr_nil)))))
                                           (fn_annot ())))
                                       (Str_macro
                                         ((fn_visibility Public)
                                           (fn_name (Id (dbg)))
                                           (fn_args
                                             ((No_label (Pat_bind (Id (msg))))
                                               (No_label (Pat_bind (Id (val))))))
                                           (fn_arity 2)
                                           (fn_body
                                             (Expr_call
                                               (Expr_var (Id (println)))
                                               ((Expr_tuple
                                                  ((Expr_unquote
                                                     (Expr_var (Id (msg))))
                                                    (Expr_unquote
                                                      (Expr_var (Id (val)))))))))
                                           (fn_annot ())))
                                       (Str_fun
                                         ((fn_visibility Public)
                                           (fn_name (Id (main)))
                                           (fn_args
                                             ((No_label (Pat_bind (Id (args))))))
                                           (fn_arity 1)
                                           (fn_body
                                             (Expr_call
                                               (Expr_var (Id (format)))
                                               ((Expr_literal
                                                  (Lit_string "~p\\n"))
                                                 (Expr_cons
                                                   (Expr_tuple
                                                     ((Expr_literal
                                                        (Lit_string input:))
                                                       (Expr_tuple
                                                         ((Expr_literal
                                                            (Lit_atom args))
                                                           (Expr_var
                                                             (Id (args)))))))
                                                   Expr_nil))))
                                           (fn_annot ()))))
