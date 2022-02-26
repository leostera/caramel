  $ echo -e "macro hello(a) { quote { unquote(a) } }\nfn f() { hello(:joe) }" > test.caramel
  $ cat test.caramel
  macro hello(a) { quote { unquote(a) } }
  fn f() { hello(:joe) }
  $ caramel parse --file test.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Private)
                                          (fn_name (Id (hello)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (a))))))
                                          (fn_arity 1)
                                          (fn_body
                                            (Expr_quote
                                              ((Quasiquote ())
                                                (Unquote (Expr_var (Id (a))))
                                                (Quasiquote ()))))
                                          (fn_annot ())))
                                       (Str_fun
                                         ((fn_visibility Private)
                                           (fn_name (Id (f))) (fn_args ())
                                           (fn_arity 0)
                                           (fn_body
                                             (Expr_literal (Lit_atom joe)))
                                           (fn_annot ()))))

  $ echo -e "macro hello(a) { quote { [ unquote(a), unquote(a) ] } }\nfn f() { hello(:joe) }" > test.caramel
  $ cat test.caramel
  macro hello(a) { quote { [ unquote(a), unquote(a) ] } }
  fn f() { hello(:joe) }
  $ caramel parse --file test.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Private)
                                          (fn_name (Id (hello)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (a))))))
                                          (fn_arity 1)
                                          (fn_body
                                            (Expr_quote
                                              ((Quasiquote (Bracket_left))
                                                (Unquote (Expr_var (Id (a))))
                                                (Quasiquote (Comma))
                                                (Unquote (Expr_var (Id (a))))
                                                (Quasiquote (Bracket_right)))))
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

  $ echo -e "macro hello(a) { quote { display(unquote(a)); [ unquote(a), unquote(a) ] } }\nfn f() { hello(:joe) }" > test.caramel
  $ cat test.caramel
  macro hello(a) { quote { display(unquote(a)); [ unquote(a), unquote(a) ] } }
  fn f() { hello(:joe) }
  $ caramel parse --file test.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Private)
                                          (fn_name (Id (hello)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (a))))))
                                          (fn_arity 1)
                                          (fn_body
                                            (Expr_quote
                                              ((Quasiquote
                                                 ((Id display) Parens_left))
                                                (Unquote (Expr_var (Id (a))))
                                                (Quasiquote
                                                  (Parens_right Semicolon
                                                    Bracket_left))
                                                (Unquote (Expr_var (Id (a))))
                                                (Quasiquote (Comma))
                                                (Unquote (Expr_var (Id (a))))
                                                (Quasiquote (Bracket_right)))))
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

  $ echo -e "macro if(a, b, c) { quote { match unquote(a) { | :true -> unquote(b) | :false ->  unquote(c) } } }\nfn f() { if(:true, :joe, :armstrong) }" > test.caramel
  $ cat test.caramel
  macro if(a, b, c) { quote { match unquote(a) { | :true -> unquote(b) | :false ->  unquote(c) } } }
  fn f() { if(:true, :joe, :armstrong) }
  $ caramel parse --file test.caramel --dump-expanded --dump-macro-env --debug
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
                                              ((Quasiquote (Match))
                                                (Unquote (Expr_var (Id (a))))
                                                (Quasiquote
                                                  (Brace_left Pipe (Atom true)
                                                    Arrow))
                                                (Unquote (Expr_var (Id (b))))
                                                (Quasiquote
                                                  (Pipe (Atom false) Arrow))
                                                (Unquote (Expr_var (Id (c))))
                                                (Quasiquote (Brace_right)))))
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

  $ echo -e "macro if(a, b, c) { match a { | :true -> quote { unquote(b) } | :false ->  quote { unquote(c) } } }\nfn f() { if(:true, :joe, :armstrong) }" > test.caramel
  $ cat test.caramel
  macro if(a, b, c) { match a { | :true -> quote { unquote(b) } | :false ->  quote { unquote(c) } } }
  fn f() { if(:true, :joe, :armstrong) }
  $ caramel parse --file test.caramel --dump-expanded --dump-macro-env --debug
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
                                                     ((Quasiquote ())
                                                       (Unquote
                                                         (Expr_var (Id (b))))
                                                       (Quasiquote ())))))
                                                ((cs_lhs
                                                   (Pat_literal
                                                     (Lit_atom false)))
                                                  (cs_rhs
                                                    (Expr_quote
                                                      ((Quasiquote ())
                                                        (Unquote
                                                          (Expr_var (Id (c))))
                                                        (Quasiquote ()))))))))
                                          (fn_annot ())))
                                       (Str_fun
                                         ((fn_visibility Private)
                                           (fn_name (Id (f))) (fn_args ())
                                           (fn_arity 0)
                                           (fn_body
                                             (Expr_literal (Lit_atom joe)))
                                           (fn_annot ()))))

  $ echo -e "pub macro debug(ast) {\n  quote {\n    pub fn type_name() {\n      unquote(ast.name)\n    }\n  }\n}\n" > test.caramel
  $ cat test.caramel
  pub macro debug(ast) {
    quote {
      pub fn type_name() {
        unquote(ast.name)
      }
    }
  }
  
  $ caramel parse --file test.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Public)
                                          (fn_name (Id (debug)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (ast))))))
                                          (fn_arity 1)
                                          (fn_body
                                            (Expr_quote
                                              ((Quasiquote
                                                 (Pub Fn (Id type_name)
                                                   Parens_left Parens_right
                                                   Brace_left))
                                                (Unquote
                                                  (Expr_field
                                                    (Expr_var (Id (ast)))
                                                    (Id (name))))
                                                (Quasiquote (Brace_right)))))
                                          (fn_annot ()))))

  $ echo -e "pub macro debug(ast) {\n  quote {\n    pub fn type_name() {\n      unquote(ast.name)\n    }\n  }\n}\n\n@derive(debug)\ntype test\n" > test.caramel
  $ cat test.caramel
  pub macro debug(ast) {
    quote {
      pub fn type_name() {
        unquote(ast.name)
      }
    }
  }
  
  @derive(debug)
  type test
  
  $ caramel parse --file test.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Public)
                                          (fn_name (Id (debug)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (ast))))))
                                          (fn_arity 1)
                                          (fn_body
                                            (Expr_quote
                                              ((Quasiquote
                                                 (Pub Fn (Id type_name)
                                                   Parens_left Parens_right
                                                   Brace_left))
                                                (Unquote
                                                  (Expr_field
                                                    (Expr_var (Id (ast)))
                                                    (Id (name))))
                                                (Quasiquote (Brace_right)))))
                                          (fn_annot ())))
                                       (Str_type
                                         ((typ_name (Id (test))) (typ_args ())
                                           (typ_desc Type_abstract)
                                           (typ_annot
                                             (((ann_name (Id (derive)))
                                                (ann_desc
                                                  ((Map (((Id (debug)) ()))))))))))
                                       (Str_fun
                                         ((fn_visibility Public)
                                           (fn_name (Id (type_name)))
                                           (fn_args ()) (fn_arity 0)
                                           (fn_body
                                             (Expr_literal (Lit_string test)))
                                           (fn_annot ()))))
