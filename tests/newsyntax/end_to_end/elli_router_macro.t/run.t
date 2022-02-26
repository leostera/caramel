  $ cat main.caramel
  macro build_path(path) {
    quote { unquote(path) }
  }
  
  macro build_routes(ctrs, not_found) {
    match ctrs {
    | [] ->
      quote {
        | _ -> unquote(not_found)()
      }
  
    | [ Constructor { name, annot }, ...rest ] ->
      quote {
        | (unquote(annot.route.method), unquote(build_path(annot.route.path))) ->
          unquote(annot.route.handler)(req, input)
  
        unquote(build_routes(rest, not_found))
      }
    }
  }
  
  pub macro router(ast) {
    let name = ast.name;
    let not_found = ast.annot.router.not_found;
  
    let routes = match ast.kind {
    | Variant { constructors } -> build_routes(constructors, not_found)
    | _ -> build_routes([], not_found)
    };
  
    quote {
      pub fn handle(req, args) {
        let meth = Request.meth(req);
        let path = Request.path(req);
  
        match (meth, path) { unquote(routes) }
      }
    }
  }
  
  @derive(router)
  @router(not_found = handle_404)
  type routes =
    | @route(method = GET, path = "/hello/{0}", handler = handle_hello)
      Hello(string)
  
  pub fn handle_hello(Hello(name)) {
    reply(200, [], "Hello, " ^ name ^ "!")
  }
  
  pub fn handle_404() {
    let html = "<!doctype html>
  <html>
    <head>
      <meta charset=\"utf-8\">
      <title>Not Found</title>
    </head>
    <body>
      <h1>Not Found</h1>
    </body>
  </html>";
  
    html |> reply(404, [])
  }
  

  $ caramel parse --file main.caramel --dump-tokens --debug
  caramel: [DEBUG] (Macro (Id build_path) Parens_left (Id path) Parens_right
                     Brace_left Quote Brace_left Unquote Parens_left (Id path)
                     Parens_right Brace_right Brace_right Macro
                     (Id build_routes) Parens_left (Id ctrs) Comma
                     (Id not_found) Parens_right Brace_left Match (Id ctrs)
                     Brace_left Pipe Bracket_left Bracket_right Arrow Quote
                     Brace_left Pipe Any Arrow Unquote Parens_left
                     (Id not_found) Parens_right Parens_left Parens_right
                     Brace_right Pipe Bracket_left (Id Constructor) Brace_left
                     (Id name) Comma (Id annot) Brace_right Comma Dot_dot_dot
                     (Id rest) Bracket_right Arrow Quote Brace_left Pipe
                     Parens_left Unquote Parens_left (Id annot.route.method)
                     Parens_right Comma Unquote Parens_left (Id build_path)
                     Parens_left (Id annot.route.path) Parens_right
                     Parens_right Parens_right Arrow Unquote Parens_left
                     (Id annot.route.handler) Parens_right Parens_left 
                     (Id req) Comma (Id input) Parens_right Unquote Parens_left
                     (Id build_routes) Parens_left (Id rest) Comma
                     (Id not_found) Parens_right Parens_right Brace_right
                     Brace_right Brace_right Pub Macro (Id router) Parens_left
                     (Id ast) Parens_right Brace_left Let (Id name) Equal
                     (Id ast.name) Semicolon Let (Id not_found) Equal
                     (Id ast.annot.router.not_found) Semicolon Let (Id routes)
                     Equal Match (Id ast.kind) Brace_left Pipe (Id Variant)
                     Brace_left (Id constructors) Brace_right Arrow
                     (Id build_routes) Parens_left (Id constructors) Comma
                     (Id not_found) Parens_right Pipe Any Arrow
                     (Id build_routes) Parens_left Bracket_left Bracket_right
                     Comma (Id not_found) Parens_right Brace_right Semicolon
                     Quote Brace_left Pub Fn (Id handle) Parens_left (Id req)
                     Comma (Id args) Parens_right Brace_left Let (Id meth)
                     Equal (Id Request.meth) Parens_left (Id req) Parens_right
                     Semicolon Let (Id path) Equal (Id Request.path)
                     Parens_left (Id req) Parens_right Semicolon Match
                     Parens_left (Id meth) Comma (Id path) Parens_right
                     Brace_left Unquote Parens_left (Id routes) Parens_right
                     Brace_right Brace_right Brace_right Brace_right At
                     (Id derive) Parens_left (Id router) Parens_right At
                     (Id router) Parens_left (Id not_found) Equal
                     (Id handle_404) Parens_right Type (Id routes) Equal Pipe
                     At (Id route) Parens_left (Id method) Equal (Id GET) Comma
                     (Id path) Equal (String /hello/{0}) Comma (Id handler)
                     Equal (Id handle_hello) Parens_right (Id Hello)
                     Parens_left (Id string) Parens_right Pub Fn
                     (Id handle_hello) Parens_left (Id Hello) Parens_left
                     (Id name) Parens_right Parens_right Brace_left (Id reply)
                     Parens_left (Integer 200) Comma Bracket_left Bracket_right
                     Comma (String "Hello, ") Caret (Id name) Caret (String !)
                     Parens_right Brace_right Pub Fn (Id handle_404)
                     Parens_left Parens_right Brace_left Let (Id html) Equal
                     (String
                        "<!doctype html>\
                       \n<html>\
                       \n  <head>\
                       \n    <meta charset=\\\"utf-8\\\">\
                       \n    <title>Not Found</title>\
                       \n  </head>\
                       \n  <body>\
                       \n    <h1>Not Found</h1>\
                       \n  </body>\
                       \n</html>")
                     Semicolon (Id html) Fun_pipe (Id reply) Parens_left
                     (Integer 404) Comma Bracket_left Bracket_right
                     Parens_right Brace_right)

  $ caramel parse --file main.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_macro
                      ((fn_visibility Private) (fn_name (Id (build_path)))
                        (fn_args ((No_label (Pat_bind (Id (path))))))
                        (fn_arity 1)
                        (fn_body
                          (Expr_quote
                            ((Quasiquote ()) (Unquote (Expr_var (Id (path))))
                              (Quasiquote ()))))
                        (fn_annot ())))
                     (Str_macro
                       ((fn_visibility Private) (fn_name (Id (build_routes)))
                         (fn_args
                           ((No_label (Pat_bind (Id (ctrs))))
                             (No_label (Pat_bind (Id (not_found))))))
                         (fn_arity 2)
                         (fn_body
                           (Expr_match (Expr_var (Id (ctrs)))
                             (((cs_lhs Pat_nil)
                                (cs_rhs
                                  (Expr_quote
                                    ((Quasiquote (Pipe Any Arrow))
                                      (Unquote (Expr_var (Id (not_found))))
                                      (Quasiquote (Parens_left Parens_right))))))
                               ((cs_lhs
                                  (Pat_cons
                                    (Pat_constructor (Id (Constructor))
                                      (Ctp_record
                                        (((Id (name)) (Pat_bind (Id (name))))
                                          ((Id (annot))
                                            (Pat_bind (Id (annot)))))
                                        Exhaustive))
                                    (Pat_bind (Id (rest)))))
                                 (cs_rhs
                                   (Expr_quote
                                     ((Quasiquote (Pipe Parens_left))
                                       (Unquote
                                         (Expr_field
                                           (Expr_field (Expr_var (Id (annot)))
                                             (Id (route)))
                                           (Id (method))))
                                       (Quasiquote (Comma))
                                       (Unquote
                                         (Expr_call
                                           (Expr_var (Id (build_path)))
                                           ((Expr_field
                                              (Expr_field
                                                (Expr_var (Id (annot)))
                                                (Id (route)))
                                              (Id (path))))))
                                       (Quasiquote (Parens_right Arrow))
                                       (Unquote
                                         (Expr_field
                                           (Expr_field (Expr_var (Id (annot)))
                                             (Id (route)))
                                           (Id (handler))))
                                       (Quasiquote
                                         (Parens_left (Id req) Comma (Id input)
                                           Parens_right))
                                       (Unquote
                                         (Expr_call
                                           (Expr_var (Id (build_routes)))
                                           ((Expr_var (Id (rest)))
                                             (Expr_var (Id (not_found))))))
                                       (Quasiquote ()))))))))
                         (fn_annot ())))
                     (Str_macro
                       ((fn_visibility Public) (fn_name (Id (router)))
                         (fn_args ((No_label (Pat_bind (Id (ast))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_let (Pat_bind (Id (name)))
                             (Expr_field (Expr_var (Id (ast))) (Id (name)))
                             (Expr_let (Pat_bind (Id (not_found)))
                               (Expr_field
                                 (Expr_field
                                   (Expr_field (Expr_var (Id (ast)))
                                     (Id (annot)))
                                   (Id (router)))
                                 (Id (not_found)))
                               (Expr_let (Pat_bind (Id (routes)))
                                 (Expr_match
                                   (Expr_field (Expr_var (Id (ast)))
                                     (Id (kind)))
                                   (((cs_lhs
                                       (Pat_constructor (Id (Variant))
                                         (Ctp_record
                                           (((Id (constructors))
                                              (Pat_bind (Id (constructors)))))
                                           Exhaustive)))
                                      (cs_rhs
                                        (Expr_call
                                          (Expr_var (Id (build_routes)))
                                          ((Expr_var (Id (constructors)))
                                            (Expr_var (Id (not_found)))))))
                                     ((cs_lhs Pat_any)
                                       (cs_rhs
                                         (Expr_call
                                           (Expr_var (Id (build_routes)))
                                           (Expr_nil
                                             (Expr_var (Id (not_found)))))))))
                                 (Expr_quote
                                   ((Quasiquote
                                      (Pub Fn (Id handle) Parens_left (Id req)
                                        Comma (Id args) Parens_right Brace_left
                                        Let (Id meth) Equal (Id Request.meth)
                                        Parens_left (Id req) Parens_right
                                        Semicolon Let (Id path) Equal
                                        (Id Request.path) Parens_left (Id req)
                                        Parens_right Semicolon Match
                                        Parens_left (Id meth) Comma (Id path)
                                        Parens_right Brace_left))
                                     (Unquote (Expr_var (Id (routes))))
                                     (Quasiquote (Brace_right Brace_right))))))))
                         (fn_annot ())))
                     (Str_type
                       ((typ_name (Id (routes))) (typ_args ())
                         (typ_desc
                           (Type_variant
                             (tyk_constructors
                               (((ctr_name (Id (Hello)))
                                  (ctr_args
                                    (Tuple ((Type_name (Id (string))))))
                                  (ctr_annot
                                    (((ann_name (Id (route)))
                                       (ann_desc
                                         ((Map
                                            (((Id (method))
                                               ((Expr_var (Id (GET)))))
                                              ((Id (path))
                                                ((Expr_literal
                                                   (Lit_string /hello/{0}))))
                                              ((Id (handler))
                                                ((Expr_var (Id (handle_hello)))))))))))))))))
                         (typ_annot
                           (((ann_name (Id (derive)))
                              (ann_desc ((Map (((Id (router)) ()))))))
                             ((ann_name (Id (router)))
                               (ann_desc
                                 ((Map
                                    (((Id (not_found))
                                       ((Expr_var (Id (handle_404))))))))))))))
                     (Str_fun
                       ((fn_visibility Public) (fn_name (Id (handle_hello)))
                         (fn_args
                           ((No_label
                              (Pat_constructor (Id (Hello))
                                (Ctp_tuple ((Pat_bind (Id (name)))))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_call (Expr_var (Id (reply)))
                             ((Expr_literal (Lit_integer 200)) Expr_nil
                               (Expr_call (Expr_var (Id (^)))
                                 ((Expr_literal (Lit_string "Hello, "))
                                   (Expr_call (Expr_var (Id (^)))
                                     ((Expr_var (Id (name)))
                                       (Expr_literal (Lit_string !)))))))))
                         (fn_annot ())))
                     (Str_fun
                       ((fn_visibility Public) (fn_name (Id (handle_404)))
                         (fn_args ()) (fn_arity 0)
                         (fn_body
                           (Expr_let (Pat_bind (Id (html)))
                             (Expr_literal
                               (Lit_string
                                  "<!doctype html>\
                                 \n<html>\
                                 \n  <head>\
                                 \n    <meta charset=\\\"utf-8\\\">\
                                 \n    <title>Not Found</title>\
                                 \n  </head>\
                                 \n  <body>\
                                 \n    <h1>Not Found</h1>\
                                 \n  </body>\
                                 \n</html>"))
                             (Expr_call (Expr_var (Id (reply)))
                               ((Expr_literal (Lit_integer 404)) Expr_nil
                                 (Expr_var (Id (html)))))))
                         (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-expanded --dump-macro-env --debug

  $ caramel parse --file main.caramel --dump-caml --debug
