  $ echo -e "macro hello(a) { quote { unquote { a } } }\nfn f() { hello(:joe) }" > test.caramel
  $ cat test.caramel
  macro hello(a) { quote { unquote { a } } }
  fn f() { hello(:joe) }
  $ caramel parse --file test.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] eval: (Expr_call
                           (Expr_lambda ((No_label (Pat_bind (Id (a)))))
                             (Expr_quote (Expr_unquote (Expr_var (Id (a))))))
                           ((Expr_literal (Lit_atom joe))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom joe))
  caramel: [DEBUG] eval: (Expr_quote (Expr_literal (Lit_atom joe)))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom joe))
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Private)
                                          (fn_name (Id (hello)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (a))))))
                                          (fn_arity 1)
                                          (fn_body
                                            (Expr_quote
                                              (Expr_unquote
                                                (Expr_var (Id (a))))))
                                          (fn_annot ())))
                                       (Str_fun
                                         ((fn_visibility Private)
                                           (fn_name (Id (f))) (fn_args ())
                                           (fn_arity 0)
                                           (fn_body
                                             (Expr_literal (Lit_atom joe)))
                                           (fn_annot ()))))

  $ echo -e "macro hello(a) { quote { [ unquote { a }, unquote { a } ] } }\nfn f() { hello(:joe) }" > test.caramel
  $ cat test.caramel
  macro hello(a) { quote { [ unquote { a }, unquote { a } ] } }
  fn f() { hello(:joe) }
  $ caramel parse --file test.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] eval: (Expr_call
                           (Expr_lambda ((No_label (Pat_bind (Id (a)))))
                             (Expr_quote
                               (Expr_cons (Expr_unquote (Expr_var (Id (a))))
                                 (Expr_cons (Expr_unquote (Expr_var (Id (a))))
                                   Expr_nil))))
                           ((Expr_literal (Lit_atom joe))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom joe))
  caramel: [DEBUG] eval: (Expr_quote
                           (Expr_cons (Expr_literal (Lit_atom joe))
                             (Expr_cons (Expr_literal (Lit_atom joe)) Expr_nil)))
  caramel: [DEBUG] eval: (Expr_cons (Expr_literal (Lit_atom joe))
                           (Expr_cons (Expr_literal (Lit_atom joe)) Expr_nil))
  caramel: [DEBUG] eval: (Expr_cons (Expr_literal (Lit_atom joe)) Expr_nil)
  caramel: [DEBUG] eval: Expr_nil
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom joe))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom joe))
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Private)
                                          (fn_name (Id (hello)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (a))))))
                                          (fn_arity 1)
                                          (fn_body
                                            (Expr_quote
                                              (Expr_cons
                                                (Expr_unquote
                                                  (Expr_var (Id (a))))
                                                (Expr_cons
                                                  (Expr_unquote
                                                    (Expr_var (Id (a))))
                                                  Expr_nil))))
                                          (fn_annot ())))
                                       (Str_fun
                                         ((fn_visibility Private)
                                           (fn_name (Id (f))) (fn_args ())
                                           (fn_arity 0)
                                           (fn_body
                                             (Expr_cons
                                               (Expr_literal (Lit_atom joe))
                                               (Expr_cons
                                                 (Expr_literal (Lit_atom joe))
                                                 Expr_nil)))
                                           (fn_annot ()))))

  $ echo -e "macro hello(a) { quote { display(unquote { a }); [ unquote { a }, unquote { a } ] } }\nfn f() { hello(:joe) }" > test.caramel
  $ cat test.caramel
  macro hello(a) { quote { display(unquote { a }); [ unquote { a }, unquote { a } ] } }
  fn f() { hello(:joe) }
  $ caramel parse --file test.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] eval: (Expr_call
                           (Expr_lambda ((No_label (Pat_bind (Id (a)))))
                             (Expr_quote
                               (Expr_seq
                                 (Expr_call (Expr_var (Id (display)))
                                   ((Expr_unquote (Expr_var (Id (a))))))
                                 (Expr_cons (Expr_unquote (Expr_var (Id (a))))
                                   (Expr_cons
                                     (Expr_unquote (Expr_var (Id (a))))
                                     Expr_nil)))))
                           ((Expr_literal (Lit_atom joe))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom joe))
  caramel: [DEBUG] eval: (Expr_quote
                           (Expr_seq
                             (Expr_call (Expr_var (Id (display)))
                               ((Expr_literal (Lit_atom joe))))
                             (Expr_cons (Expr_literal (Lit_atom joe))
                               (Expr_cons (Expr_literal (Lit_atom joe))
                                 Expr_nil))))
  caramel: [DEBUG] eval: (Expr_seq
                           (Expr_call (Expr_var (Id (display)))
                             ((Expr_literal (Lit_atom joe))))
                           (Expr_cons (Expr_literal (Lit_atom joe))
                             (Expr_cons (Expr_literal (Lit_atom joe)) Expr_nil)))
  caramel: [DEBUG] eval: (Expr_cons (Expr_literal (Lit_atom joe))
                           (Expr_cons (Expr_literal (Lit_atom joe)) Expr_nil))
  caramel: [DEBUG] eval: (Expr_cons (Expr_literal (Lit_atom joe)) Expr_nil)
  caramel: [DEBUG] eval: Expr_nil
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom joe))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom joe))
  caramel: [DEBUG] eval: (Expr_call (Expr_var (Id (display)))
                           ((Expr_literal (Lit_atom joe))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom joe))
  caramel: [DEBUG] eval: (Expr_var (Id (display)))
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Private)
                                          (fn_name (Id (hello)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (a))))))
                                          (fn_arity 1)
                                          (fn_body
                                            (Expr_quote
                                              (Expr_seq
                                                (Expr_call
                                                  (Expr_var (Id (display)))
                                                  ((Expr_unquote
                                                     (Expr_var (Id (a))))))
                                                (Expr_cons
                                                  (Expr_unquote
                                                    (Expr_var (Id (a))))
                                                  (Expr_cons
                                                    (Expr_unquote
                                                      (Expr_var (Id (a))))
                                                    Expr_nil)))))
                                          (fn_annot ())))
                                       (Str_fun
                                         ((fn_visibility Private)
                                           (fn_name (Id (f))) (fn_args ())
                                           (fn_arity 0)
                                           (fn_body
                                             (Expr_seq
                                               (Expr_call
                                                 (Expr_var (Id (display)))
                                                 ((Expr_literal (Lit_atom joe))))
                                               (Expr_cons
                                                 (Expr_literal (Lit_atom joe))
                                                 (Expr_cons
                                                   (Expr_literal
                                                     (Lit_atom joe))
                                                   Expr_nil))))
                                           (fn_annot ()))))

  $ echo -e "macro if(a, b, c) { quote { match unquote { a } { | :true -> unquote { b } | :false ->  unquote { c } } } }\nfn f() { if(:true, :joe, :armstrong) }" > test.caramel
  $ cat test.caramel
  macro if(a, b, c) { quote { match unquote { a } { | :true -> unquote { b } | :false ->  unquote { c } } } }
  fn f() { if(:true, :joe, :armstrong) }
  $ caramel parse --file test.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] eval: (Expr_call
                           (Expr_lambda
                             ((No_label (Pat_bind (Id (a))))
                               (No_label (Pat_bind (Id (b))))
                               (No_label (Pat_bind (Id (c)))))
                             (Expr_quote
                               (Expr_match (Expr_unquote (Expr_var (Id (a))))
                                 (((cs_lhs (Pat_literal (Lit_atom true)))
                                    (cs_rhs (Expr_unquote (Expr_var (Id (b))))))
                                   ((cs_lhs (Pat_literal (Lit_atom false)))
                                     (cs_rhs
                                       (Expr_unquote (Expr_var (Id (c))))))))))
                           ((Expr_literal (Lit_atom true))
                             (Expr_literal (Lit_atom joe))
                             (Expr_literal (Lit_atom armstrong))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom true))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom joe))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom armstrong))
  caramel: [DEBUG] eval: (Expr_quote
                           (Expr_match (Expr_literal (Lit_atom true))
                             (((cs_lhs (Pat_literal (Lit_atom true)))
                                (cs_rhs (Expr_literal (Lit_atom joe))))
                               ((cs_lhs (Pat_literal (Lit_atom false)))
                                 (cs_rhs (Expr_literal (Lit_atom armstrong)))))))
  caramel: [DEBUG] eval: (Expr_match (Expr_literal (Lit_atom true))
                           (((cs_lhs (Pat_literal (Lit_atom true)))
                              (cs_rhs (Expr_literal (Lit_atom joe))))
                             ((cs_lhs (Pat_literal (Lit_atom false)))
                               (cs_rhs (Expr_literal (Lit_atom armstrong))))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom joe))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom armstrong))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom true))
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Private)
                                          (fn_name (Id (if)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (a))))
                                              (No_label (Pat_bind (Id (b))))
                                              (No_label (Pat_bind (Id (c))))))
                                          (fn_arity 3)
                                          (fn_body
                                            (Expr_quote
                                              (Expr_match
                                                (Expr_unquote
                                                  (Expr_var (Id (a))))
                                                (((cs_lhs
                                                    (Pat_literal
                                                      (Lit_atom true)))
                                                   (cs_rhs
                                                     (Expr_unquote
                                                       (Expr_var (Id (b))))))
                                                  ((cs_lhs
                                                     (Pat_literal
                                                       (Lit_atom false)))
                                                    (cs_rhs
                                                      (Expr_unquote
                                                        (Expr_var (Id (c))))))))))
                                          (fn_annot ())))
                                       (Str_fun
                                         ((fn_visibility Private)
                                           (fn_name (Id (f))) (fn_args ())
                                           (fn_arity 0)
                                           (fn_body
                                             (Expr_match
                                               (Expr_literal (Lit_atom true))
                                               (((cs_lhs
                                                   (Pat_literal
                                                     (Lit_atom true)))
                                                  (cs_rhs
                                                    (Expr_literal
                                                      (Lit_atom joe))))
                                                 ((cs_lhs
                                                    (Pat_literal
                                                      (Lit_atom false)))
                                                   (cs_rhs
                                                     (Expr_literal
                                                       (Lit_atom armstrong)))))))
                                           (fn_annot ()))))

  $ echo -e "macro if(a, b, c) { match a { | :true -> quote { unquote { b } } | :false ->  quote { unquote { c } } } }\nfn f() { if(:true, :joe, :armstrong) }" > test.caramel
  $ cat test.caramel
  macro if(a, b, c) { match a { | :true -> quote { unquote { b } } | :false ->  quote { unquote { c } } } }
  fn f() { if(:true, :joe, :armstrong) }
  $ caramel parse --file test.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] eval: (Expr_call
                           (Expr_lambda
                             ((No_label (Pat_bind (Id (a))))
                               (No_label (Pat_bind (Id (b))))
                               (No_label (Pat_bind (Id (c)))))
                             (Expr_match (Expr_var (Id (a)))
                               (((cs_lhs (Pat_literal (Lit_atom true)))
                                  (cs_rhs
                                    (Expr_quote
                                      (Expr_unquote (Expr_var (Id (b)))))))
                                 ((cs_lhs (Pat_literal (Lit_atom false)))
                                   (cs_rhs
                                     (Expr_quote
                                       (Expr_unquote (Expr_var (Id (c))))))))))
                           ((Expr_literal (Lit_atom true))
                             (Expr_literal (Lit_atom joe))
                             (Expr_literal (Lit_atom armstrong))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom true))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom joe))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom armstrong))
  caramel: [DEBUG] eval: (Expr_match (Expr_literal (Lit_atom true))
                           (((cs_lhs (Pat_literal (Lit_atom true)))
                              (cs_rhs
                                (Expr_quote (Expr_literal (Lit_atom joe)))))
                             ((cs_lhs (Pat_literal (Lit_atom false)))
                               (cs_rhs
                                 (Expr_quote
                                   (Expr_literal (Lit_atom armstrong)))))))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom true))
  caramel: [DEBUG] match (Pat_literal (Lit_atom true)) with (Expr_literal
                                                              (Lit_atom true))
  caramel: [DEBUG] eval: (Expr_quote (Expr_literal (Lit_atom joe)))
  caramel: [DEBUG] eval: (Expr_literal (Lit_atom joe))
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Private)
                                          (fn_name (Id (if)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (a))))
                                              (No_label (Pat_bind (Id (b))))
                                              (No_label (Pat_bind (Id (c))))))
                                          (fn_arity 3)
                                          (fn_body
                                            (Expr_match (Expr_var (Id (a)))
                                              (((cs_lhs
                                                  (Pat_literal (Lit_atom true)))
                                                 (cs_rhs
                                                   (Expr_quote
                                                     (Expr_unquote
                                                       (Expr_var (Id (b)))))))
                                                ((cs_lhs
                                                   (Pat_literal
                                                     (Lit_atom false)))
                                                  (cs_rhs
                                                    (Expr_quote
                                                      (Expr_unquote
                                                        (Expr_var (Id (c))))))))))
                                          (fn_annot ())))
                                       (Str_fun
                                         ((fn_visibility Private)
                                           (fn_name (Id (f))) (fn_args ())
                                           (fn_arity 0)
                                           (fn_body
                                             (Expr_literal (Lit_atom joe)))
                                           (fn_annot ()))))
