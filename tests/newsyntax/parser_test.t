  $ echo "" > test.caramel
  $ cat test.caramel
  
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ()

  $ echo -e "\n" > test.caramel
  $ cat test.caramel
  
  
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ()

  $ echo -e "type t = | Print { message: string }" > test.caramel
  $ cat test.caramel
  type t = | Print { message: string }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print)))
                                 (ctr_args
                                   (Record
                                     (((lbl_name (Id (message)))
                                        (lbl_type (Type_name (Id (string))))
                                        (lbl_annot ())))))
                                 (ctr_annot ()))))))
                        (typ_annot ()))))

  $ echo -e "type t = | Print { message: string, }" > test.caramel
  $ cat test.caramel
  type t = | Print { message: string, }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print)))
                                 (ctr_args
                                   (Record
                                     (((lbl_name (Id (message)))
                                        (lbl_type (Type_name (Id (string))))
                                        (lbl_annot ())))))
                                 (ctr_annot ()))))))
                        (typ_annot ()))))

  $ echo -e "type t = | Print { message: string, other: field }" > test.caramel
  $ cat test.caramel
  type t = | Print { message: string, other: field }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print)))
                                 (ctr_args
                                   (Record
                                     (((lbl_name (Id (message)))
                                        (lbl_type (Type_name (Id (string))))
                                        (lbl_annot ()))
                                       ((lbl_name (Id (other)))
                                         (lbl_type (Type_name (Id (field))))
                                         (lbl_annot ())))))
                                 (ctr_annot ()))))))
                        (typ_annot ()))))

  $ echo -e "type t = | Print { message: string, other: field, and_third_field: no_way, }" > test.caramel
  $ cat test.caramel
  type t = | Print { message: string, other: field, and_third_field: no_way, }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print)))
                                 (ctr_args
                                   (Record
                                     (((lbl_name (Id (message)))
                                        (lbl_type (Type_name (Id (string))))
                                        (lbl_annot ()))
                                       ((lbl_name (Id (other)))
                                         (lbl_type (Type_name (Id (field))))
                                         (lbl_annot ()))
                                       ((lbl_name (Id (and_third_field)))
                                         (lbl_type (Type_name (Id (no_way))))
                                         (lbl_annot ())))))
                                 (ctr_annot ()))))))
                        (typ_annot ()))))

  $ echo -e "type t =\n\t| Print (string, int, bool)\n\t| Write { file: File.t, contents: string}" > test.caramel
  $ cat test.caramel
  type t =
  	| Print (string, int, bool)
  	| Write { file: File.t, contents: string}
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print)))
                                 (ctr_args
                                   (Tuple
                                     ((Type_name (Id (string)))
                                       (Type_name (Id (int)))
                                       (Type_name (Id (bool))))))
                                 (ctr_annot ()))
                                ((ctr_name (Id (Write)))
                                  (ctr_args
                                    (Record
                                      (((lbl_name (Id (file)))
                                         (lbl_type (Type_name (Id (File t))))
                                         (lbl_annot ()))
                                        ((lbl_name (Id (contents)))
                                          (lbl_type (Type_name (Id (string))))
                                          (lbl_annot ())))))
                                  (ctr_annot ()))))))
                        (typ_annot ()))))

  $ echo -e "type t = | Print" > test.caramel
  $ cat test.caramel
  type t = | Print
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print))) (ctr_args (Tuple ()))
                                 (ctr_annot ()))))))
                        (typ_annot ()))))

  $ echo -e "type t = | Print\ntype t2 = | Write" > test.caramel
  $ cat test.caramel
  type t = | Print
  type t2 = | Write
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print))) (ctr_args (Tuple ()))
                                 (ctr_annot ()))))))
                        (typ_annot ())))
                     (Str_type
                       ((typ_name (Id (t2))) (typ_args ())
                         (typ_desc
                           (Type_variant
                             (tyk_constructors
                               (((ctr_name (Id (Write))) (ctr_args (Tuple ()))
                                  (ctr_annot ()))))))
                         (typ_annot ()))))

  $ echo -e "@clipper\ntype t = | Print" > test.caramel
  $ cat test.caramel
  @clipper
  type t = | Print
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print))) (ctr_args (Tuple ()))
                                 (ctr_annot ()))))))
                        (typ_annot (((ann_name (Id (clipper))) (ann_desc ())))))))

  $ echo -e "@clipper(name)\ntype t = | Print" > test.caramel
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print))) (ctr_args (Tuple ()))
                                 (ctr_annot ()))))))
                        (typ_annot
                          (((ann_name (Id (clipper)))
                             (ann_desc ((Map (((Id (name)) ())))))))))))

  $ echo -e "@clipper(\n\tname = \"stuff\"\n)\ntype t = | Print" > test.caramel
  $ cat test.caramel
  @clipper(
  	name = "stuff"
  )
  type t = | Print
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print))) (ctr_args (Tuple ()))
                                 (ctr_annot ()))))))
                        (typ_annot
                          (((ann_name (Id (clipper)))
                             (ann_desc
                               ((Map
                                  (((Id (name))
                                     ((Expr_literal (Lit_string stuff))))))))))))))

  $ echo -e "@clipper(\n\tname = \"stuff\",\n\tdescription = \"a cli for stuff!\"\n)\ntype t = | Print" > test.caramel
  $ cat test.caramel
  @clipper(
  	name = "stuff",
  	description = "a cli for stuff!"
  )
  type t = | Print
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print))) (ctr_args (Tuple ()))
                                 (ctr_annot ()))))))
                        (typ_annot
                          (((ann_name (Id (clipper)))
                             (ann_desc
                               ((Map
                                  (((Id (name))
                                     ((Expr_literal (Lit_string stuff))))
                                    ((Id (description))
                                      ((Expr_literal
                                         (Lit_string "a cli for stuff!"))))))))))))))

  $ echo -e "type t = | Print { @clipper(short = \"m\") message: string }" > test.caramel
  $ cat test.caramel
  type t = | Print { @clipper(short = "m") message: string }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print)))
                                 (ctr_args
                                   (Record
                                     (((lbl_name (Id (message)))
                                        (lbl_type (Type_name (Id (string))))
                                        (lbl_annot
                                          (((ann_name (Id (clipper)))
                                             (ann_desc
                                               ((Map
                                                  (((Id (short))
                                                     ((Expr_literal
                                                        (Lit_string m)))))))))))))))
                                 (ctr_annot ()))))))
                        (typ_annot ()))))

  $ echo -e "fn hello() { joe }" > test.caramel
  $ cat test.caramel
  fn hello() { joe }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Private) (fn_name (Id (hello)))
                        (fn_args ()) (fn_arity 0)
                        (fn_body (Expr_var (Id (joe)))) (fn_annot ()))))

  $ echo -e "pub fn hello() { :joe }" > test.caramel
  $ cat test.caramel
  pub fn hello() { :joe }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (hello)))
                        (fn_args ()) (fn_arity 0)
                        (fn_body (Expr_literal (Lit_atom joe))) (fn_annot ()))))

  $ echo -e "pub fn hello(_my, _good) { :joe }" > test.caramel
  $ cat test.caramel
  pub fn hello(_my, _good) { :joe }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (hello)))
                        (fn_args
                          ((No_label (Pat_bind (Id (_my))))
                            (No_label (Pat_bind (Id (_good))))))
                        (fn_arity 2) (fn_body (Expr_literal (Lit_atom joe)))
                        (fn_annot ()))))

  $ echo -e "pub fn fst(x, _) { x }" > test.caramel
  $ cat test.caramel
  pub fn fst(x, _) { x }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (fst)))
                        (fn_args
                          ((No_label (Pat_bind (Id (x)))) (No_label Pat_any)))
                        (fn_arity 2) (fn_body (Expr_var (Id (x))))
                        (fn_annot ()))))

  $ echo -e "pub fn snd(_, y) { y }" > test.caramel
  $ cat test.caramel
  pub fn snd(_, y) { y }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (snd)))
                        (fn_args
                          ((No_label Pat_any) (No_label (Pat_bind (Id (y))))))
                        (fn_arity 2) (fn_body (Expr_var (Id (y))))
                        (fn_annot ()))))

  $ echo -e "pub fn pair(x, y) { (x, y) }" > test.caramel
  $ cat test.caramel
  pub fn pair(x, y) { (x, y) }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (pair)))
                        (fn_args
                          ((No_label (Pat_bind (Id (x))))
                            (No_label (Pat_bind (Id (y))))))
                        (fn_arity 2)
                        (fn_body
                          (Expr_tuple
                            ((Expr_var (Id (x))) (Expr_var (Id (y))))))
                        (fn_annot ()))))

  $ echo -e "pub fn double_pair(x, y) { ((x, x), (y, y)) }" > test.caramel
  $ cat test.caramel
  pub fn double_pair(x, y) { ((x, x), (y, y)) }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (double_pair)))
                        (fn_args
                          ((No_label (Pat_bind (Id (x))))
                            (No_label (Pat_bind (Id (y))))))
                        (fn_arity 2)
                        (fn_body
                          (Expr_tuple
                            ((Expr_tuple
                               ((Expr_var (Id (x))) (Expr_var (Id (x)))))
                              (Expr_tuple
                                ((Expr_var (Id (y))) (Expr_var (Id (y))))))))
                        (fn_annot ()))))

  $ echo -e "pub fn pair_list(x, y) { [x, y] }" > test.caramel
  $ cat test.caramel
  pub fn pair_list(x, y) { [x, y] }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (pair_list)))
                        (fn_args
                          ((No_label (Pat_bind (Id (x))))
                            (No_label (Pat_bind (Id (y))))))
                        (fn_arity 2)
                        (fn_body
                          (Expr_cons (Expr_var (Id (x)))
                            (Expr_cons (Expr_var (Id (y))) Expr_nil)))
                        (fn_annot ()))))

  $ echo -e "pub fn nil() { [] }" > test.caramel
  $ cat test.caramel
  pub fn nil() { [] }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (nil))) (fn_args ())
                        (fn_arity 0) (fn_body Expr_nil) (fn_annot ()))))

  $ echo -e "pub fn cons(x, y) { [x, ...y] }" > test.caramel
  $ cat test.caramel
  pub fn cons(x, y) { [x, ...y] }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (cons)))
                        (fn_args
                          ((No_label (Pat_bind (Id (x))))
                            (No_label (Pat_bind (Id (y))))))
                        (fn_arity 2)
                        (fn_body
                          (Expr_cons (Expr_var (Id (x))) (Expr_var (Id (y)))))
                        (fn_annot ()))))

  $ echo -e "pub fn cons_and_splat(x, y) { [x, x, x, ...y] }" > test.caramel
  $ cat test.caramel
  pub fn cons_and_splat(x, y) { [x, x, x, ...y] }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (cons_and_splat)))
                        (fn_args
                          ((No_label (Pat_bind (Id (x))))
                            (No_label (Pat_bind (Id (y))))))
                        (fn_arity 2)
                        (fn_body
                          (Expr_cons (Expr_var (Id (x)))
                            (Expr_cons (Expr_var (Id (x)))
                              (Expr_cons (Expr_var (Id (x)))
                                (Expr_var (Id (y)))))))
                        (fn_annot ()))))

  $ echo -e "pub fn cons_pairs(x, y) { [(x, x), ...y] }" > test.caramel
  $ cat test.caramel
  pub fn cons_pairs(x, y) { [(x, x), ...y] }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (cons_pairs)))
                        (fn_args
                          ((No_label (Pat_bind (Id (x))))
                            (No_label (Pat_bind (Id (y))))))
                        (fn_arity 2)
                        (fn_body
                          (Expr_cons
                            (Expr_tuple
                              ((Expr_var (Id (x))) (Expr_var (Id (x)))))
                            (Expr_var (Id (y)))))
                        (fn_annot ()))))

  $ echo -e "pub fn f() { [1, [args | [ args | []]], 2 ] }" > test.caramel
  $ cat test.caramel
  pub fn f() { [1, [args | [ args | []]], 2 ] }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [ERROR] Parsing error: (Expected_symbol
                                    (expected
                                      (One_of (Dot_dot_dot Bracket_right)))
                                    (found
                                      ((start_pos
                                         ((filename test.caramel) (line_num 1)
                                           (col_num 24) (offset 23)))
                                        (end_pos
                                          ((filename test.caramel) (line_num 1)
                                            (col_num 25) (offset 24)))
                                        (token Pipe))))
  [2]

  $ echo -e "pub fn f() {\n  match n {\n  | [] -> :empty\n  | _ -> :non_empty\n  }\n}" > test.caramel
  $ cat test.caramel
  pub fn f() {
    match n {
    | [] -> :empty
    | _ -> :non_empty
    }
  }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (f))) (fn_args ())
                        (fn_arity 0)
                        (fn_body
                          (Expr_match (Expr_var (Id (n)))
                            (((cs_lhs Pat_nil)
                               (cs_rhs (Expr_literal (Lit_atom empty))))
                              ((cs_lhs Pat_any)
                                (cs_rhs (Expr_literal (Lit_atom non_empty)))))))
                        (fn_annot ()))))

  $ echo -e "pub fn f() {\n  match n {\n  | () -> :empty\n  | (_, (_, _)) -> :non_empty\n  }\n}" > test.caramel
  $ cat test.caramel
  pub fn f() {
    match n {
    | () -> :empty
    | (_, (_, _)) -> :non_empty
    }
  }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (f))) (fn_args ())
                        (fn_arity 0)
                        (fn_body
                          (Expr_match (Expr_var (Id (n)))
                            (((cs_lhs (Pat_tuple ()))
                               (cs_rhs (Expr_literal (Lit_atom empty))))
                              ((cs_lhs
                                 (Pat_tuple
                                   (Pat_any (Pat_tuple (Pat_any Pat_any)))))
                                (cs_rhs (Expr_literal (Lit_atom non_empty)))))))
                        (fn_annot ()))))

  $ echo -e "pub fn f() {\n  match n {\n  | [(x, _), (y, _), ...z] -> :empty\n  | (a, (b, c)) -> :non_empty\n  }\n}" > test.caramel
  $ cat test.caramel
  pub fn f() {
    match n {
    | [(x, _), (y, _), ...z] -> :empty
    | (a, (b, c)) -> :non_empty
    }
  }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (f))) (fn_args ())
                        (fn_arity 0)
                        (fn_body
                          (Expr_match (Expr_var (Id (n)))
                            (((cs_lhs
                                (Pat_cons
                                  (Pat_tuple ((Pat_bind (Id (x))) Pat_any))
                                  (Pat_cons
                                    (Pat_tuple ((Pat_bind (Id (y))) Pat_any))
                                    (Pat_bind (Id (z))))))
                               (cs_rhs (Expr_literal (Lit_atom empty))))
                              ((cs_lhs
                                 (Pat_tuple
                                   ((Pat_bind (Id (a)))
                                     (Pat_tuple
                                       ((Pat_bind (Id (b)))
                                         (Pat_bind (Id (c))))))))
                                (cs_rhs (Expr_literal (Lit_atom non_empty)))))))
                        (fn_annot ()))))

  $ echo -e "pub fn fix(n) {\n  fix(n)\n}" > test.caramel
  $ cat test.caramel
  pub fn fix(n) {
    fix(n)
  }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (fix)))
                        (fn_args ((No_label (Pat_bind (Id (n)))))) (fn_arity 1)
                        (fn_body
                          (Expr_call (Expr_var (Id (fix)))
                            ((Expr_var (Id (n))))))
                        (fn_annot ()))))

  $ echo -e "pub fn fix(n) {\n  fix(n);\n  fix(n);\n  fix(n)\n}" > test.caramel
  $ cat test.caramel
  pub fn fix(n) {
    fix(n);
    fix(n);
    fix(n)
  }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_fun
                      ((fn_visibility Public) (fn_name (Id (fix)))
                        (fn_args ((No_label (Pat_bind (Id (n)))))) (fn_arity 1)
                        (fn_body
                          (Expr_seq
                            (Expr_call (Expr_var (Id (fix)))
                              ((Expr_var (Id (n)))))
                            (Expr_seq
                              (Expr_call (Expr_var (Id (fix)))
                                ((Expr_var (Id (n)))))
                              (Expr_call (Expr_var (Id (fix)))
                                ((Expr_var (Id (n))))))))
                        (fn_annot ()))))

  $ echo -e "external f: unit -> 'a = \"hello\"" > test.caramel
  $ cat test.caramel
  external f: unit -> 'a = "hello"
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_extern
                      ((ext_name (Id (f)))
                        (ext_type
                          (Type_arrow (Type_name (Id (unit))) (Type_var a)))
                        (ext_symbol hello) (ext_visibility Private)
                        (ext_annot ()))))

  $ echo -e "external format: string -> list<'a> -> unit = \"io:format\"" > test.caramel
  $ cat test.caramel
  external format: string -> list<'a> -> unit = "io:format"
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_extern
                      ((ext_name (Id (format)))
                        (ext_type
                          (Type_arrow (Type_name (Id (string)))
                            (Type_arrow (Type_apply (Id (list)) ((Type_var a)))
                              (Type_name (Id (unit))))))
                        (ext_symbol io:format) (ext_visibility Private)
                        (ext_annot ()))))

  $ echo -e "macro hello(a) { quote { unquote(a) } }\nfn f() { hello(:joe) }" > test.caramel
  $ cat test.caramel
  macro hello(a) { quote { unquote(a) } }
  fn f() { hello(:joe) }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_macro
                      ((fn_visibility Private) (fn_name (Id (hello)))
                        (fn_args ((No_label (Pat_bind (Id (a)))))) (fn_arity 1)
                        (fn_body
                          (Expr_quote
                            ((Quasiquote ()) (Unquote (Expr_var (Id (a))))
                              (Quasiquote ()))))
                        (fn_annot ())))
                     (Str_fun
                       ((fn_visibility Private) (fn_name (Id (f))) (fn_args ())
                         (fn_arity 0)
                         (fn_body
                           (Expr_call (Expr_var (Id (hello)))
                             ((Expr_literal (Lit_atom joe)))))
                         (fn_annot ()))))

  $ echo -e "macro hello(a) { quote { [ unquote(a), unquote(a) ] } }\nfn f() { hello(:joe) }" > test.caramel
  $ cat test.caramel
  macro hello(a) { quote { [ unquote(a), unquote(a) ] } }
  fn f() { hello(:joe) }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_macro
                      ((fn_visibility Private) (fn_name (Id (hello)))
                        (fn_args ((No_label (Pat_bind (Id (a)))))) (fn_arity 1)
                        (fn_body
                          (Expr_quote
                            ((Quasiquote (Bracket_left))
                              (Unquote (Expr_var (Id (a))))
                              (Quasiquote (Comma))
                              (Unquote (Expr_var (Id (a))))
                              (Quasiquote (Bracket_right)))))
                        (fn_annot ())))
                     (Str_fun
                       ((fn_visibility Private) (fn_name (Id (f))) (fn_args ())
                         (fn_arity 0)
                         (fn_body
                           (Expr_call (Expr_var (Id (hello)))
                             ((Expr_literal (Lit_atom joe)))))
                         (fn_annot ()))))

  $ echo -e "macro hello(a) { quote { display(unquote(a)); [ unquote(a), unquote(a) ] } }\nfn f() { hello(:joe) }" > test.caramel
  $ cat test.caramel
  macro hello(a) { quote { display(unquote(a)); [ unquote(a), unquote(a) ] } }
  fn f() { hello(:joe) }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_macro
                      ((fn_visibility Private) (fn_name (Id (hello)))
                        (fn_args ((No_label (Pat_bind (Id (a)))))) (fn_arity 1)
                        (fn_body
                          (Expr_quote
                            ((Quasiquote ((Id display) Parens_left))
                              (Unquote (Expr_var (Id (a))))
                              (Quasiquote
                                (Parens_right Semicolon Bracket_left))
                              (Unquote (Expr_var (Id (a))))
                              (Quasiquote (Comma))
                              (Unquote (Expr_var (Id (a))))
                              (Quasiquote (Bracket_right)))))
                        (fn_annot ())))
                     (Str_fun
                       ((fn_visibility Private) (fn_name (Id (f))) (fn_args ())
                         (fn_arity 0)
                         (fn_body
                           (Expr_call (Expr_var (Id (hello)))
                             ((Expr_literal (Lit_atom joe)))))
                         (fn_annot ()))))

  $ echo -e "macro if(a, b, c) { quote { match unquote(a) { | :true -> unquote(b) | :false ->  unquote(c) } } }\nfn f() { if(:true, :joe, :armstrong) }" > test.caramel
  $ cat test.caramel
  macro if(a, b, c) { quote { match unquote(a) { | :true -> unquote(b) | :false ->  unquote(c) } } }
  fn f() { if(:true, :joe, :armstrong) }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_macro
                      ((fn_visibility Private) (fn_name (Id (if)))
                        (fn_args
                          ((No_label (Pat_bind (Id (a))))
                            (No_label (Pat_bind (Id (b))))
                            (No_label (Pat_bind (Id (c))))))
                        (fn_arity 3)
                        (fn_body
                          (Expr_quote
                            ((Quasiquote (Match)) (Unquote (Expr_var (Id (a))))
                              (Quasiquote (Brace_left Pipe (Atom true) Arrow))
                              (Unquote (Expr_var (Id (b))))
                              (Quasiquote (Pipe (Atom false) Arrow))
                              (Unquote (Expr_var (Id (c))))
                              (Quasiquote (Brace_right)))))
                        (fn_annot ())))
                     (Str_fun
                       ((fn_visibility Private) (fn_name (Id (f))) (fn_args ())
                         (fn_arity 0)
                         (fn_body
                           (Expr_call (Expr_var (Id (if)))
                             ((Expr_literal (Lit_atom true))
                               (Expr_literal (Lit_atom joe))
                               (Expr_literal (Lit_atom armstrong)))))
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
  
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_macro
                      ((fn_visibility Public) (fn_name (Id (debug)))
                        (fn_args ((No_label (Pat_bind (Id (ast))))))
                        (fn_arity 1)
                        (fn_body
                          (Expr_quote
                            ((Quasiquote
                               (Pub Fn (Id type_name) Parens_left Parens_right
                                 Brace_left))
                              (Unquote
                                (Expr_field (Expr_var (Id (ast))) (Id (name))))
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
  
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_macro
                      ((fn_visibility Public) (fn_name (Id (debug)))
                        (fn_args ((No_label (Pat_bind (Id (ast))))))
                        (fn_arity 1)
                        (fn_body
                          (Expr_quote
                            ((Quasiquote
                               (Pub Fn (Id type_name) Parens_left Parens_right
                                 Brace_left))
                              (Unquote
                                (Expr_field (Expr_var (Id (ast))) (Id (name))))
                              (Quasiquote (Brace_right)))))
                        (fn_annot ())))
                     (Str_type
                       ((typ_name (Id (test))) (typ_args ())
                         (typ_desc Type_abstract)
                         (typ_annot
                           (((ann_name (Id (derive)))
                              (ann_desc ((Map (((Id (debug)) ())))))))))))

  $ echo -e "pub fn f() { a |> f |> g }" > test.caramel
  $ cat test.caramel
  type t = | Print { message: string }
  $ caramel parse --file test.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_type
                      ((typ_name (Id (t))) (typ_args ())
                        (typ_desc
                          (Type_variant
                            (tyk_constructors
                              (((ctr_name (Id (Print)))
                                 (ctr_args
                                   (Record
                                     (((lbl_name (Id (message)))
                                        (lbl_type (Type_name (Id (string))))
                                        (lbl_annot ())))))
                                 (ctr_annot ()))))))
                        (typ_annot ()))))
