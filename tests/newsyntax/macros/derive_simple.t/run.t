  $ cat main.caramel
  pub macro say_hi(item) {
    quote {
      pub fn say_hi() { :hello }
    }
  }
  
  pub macro say_bye(item) {
    quote {
      pub fn say_bye() { :goodbye }
    }
  }
  
  @derive(say_hi, say_bye)
  type t

  $ caramel parse --file main.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Macro (Id say_hi) Parens_left (Id item) Parens_right
                     Brace_left Quote Brace_left Pub Fn (Id say_hi) Parens_left
                     Parens_right Brace_left (Atom hello) Brace_right
                     Brace_right Brace_right Pub Macro (Id say_bye) Parens_left
                     (Id item) Parens_right Brace_left Quote Brace_left Pub Fn
                     (Id say_bye) Parens_left Parens_right Brace_left
                     (Atom goodbye) Brace_right Brace_right Brace_right At
                     (Id derive) Parens_left (Id say_hi) Comma (Id say_bye)
                     Parens_right Type (Id t))

  $ caramel parse --file main.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_macro
                      ((fn_visibility Public) (fn_name (Id (say_hi)))
                        (fn_args ((No_label (Pat_bind (Id (item))))))
                        (fn_arity 1)
                        (fn_body
                          (Expr_quote
                            ((Quasiquote
                               (Pub Fn (Id say_hi) Parens_left Parens_right
                                 Brace_left (Atom hello) Brace_right)))))
                        (fn_annot ())))
                     (Str_macro
                       ((fn_visibility Public) (fn_name (Id (say_bye)))
                         (fn_args ((No_label (Pat_bind (Id (item))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_quote
                             ((Quasiquote
                                (Pub Fn (Id say_bye) Parens_left Parens_right
                                  Brace_left (Atom goodbye) Brace_right)))))
                         (fn_annot ())))
                     (Str_type
                       ((typ_name (Id (t))) (typ_args ())
                         (typ_desc Type_abstract)
                         (typ_annot
                           (((ann_name (Id (derive)))
                              (ann_desc
                                ((Map (((Id (say_hi)) ()) ((Id (say_bye)) ())))))))))))

  $ caramel parse --file main.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Public)
                                          (fn_name (Id (say_hi)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (item))))))
                                          (fn_arity 1)
                                          (fn_body
                                            (Expr_quote
                                              ((Quasiquote
                                                 (Pub Fn (Id say_hi)
                                                   Parens_left Parens_right
                                                   Brace_left (Atom hello)
                                                   Brace_right)))))
                                          (fn_annot ())))
                                       (Str_macro
                                         ((fn_visibility Public)
                                           (fn_name (Id (say_bye)))
                                           (fn_args
                                             ((No_label (Pat_bind (Id (item))))))
                                           (fn_arity 1)
                                           (fn_body
                                             (Expr_quote
                                               ((Quasiquote
                                                  (Pub Fn (Id say_bye)
                                                    Parens_left Parens_right
                                                    Brace_left (Atom goodbye)
                                                    Brace_right)))))
                                           (fn_annot ())))
                                       (Str_type
                                         ((typ_name (Id (t))) (typ_args ())
                                           (typ_desc Type_abstract)
                                           (typ_annot
                                             (((ann_name (Id (derive)))
                                                (ann_desc
                                                  ((Map
                                                     (((Id (say_hi)) ())
                                                       ((Id (say_bye)) ()))))))))))
                                       (Str_fun
                                         ((fn_visibility Public)
                                           (fn_name (Id (say_bye)))
                                           (fn_args ()) (fn_arity 0)
                                           (fn_body
                                             (Expr_literal (Lit_atom goodbye)))
                                           (fn_annot ())))
                                       (Str_fun
                                         ((fn_visibility Public)
                                           (fn_name (Id (say_hi))) (fn_args ())
                                           (fn_arity 0)
                                           (fn_body
                                             (Expr_literal (Lit_atom hello)))
                                           (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-caml --debug
  caramel: [DEBUG] type t
                   let rec say_bye () = `goodbye
                   let rec say_hi () = `hello

  $ caramel compile main.caramel --new-syntax --debug
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (main.caramel)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax true) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file main.caramel)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing main.caramel.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing main.caramel.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing main.caramel.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing main.caramel.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat main.caramel.lambda
  (letrec (say_bye/4 (function param/5 -929147503))
    (letrec (say_hi/6 (function param/7 616641298))
      (makeblock 0 say_bye/4 say_hi/6)))

  $ cat main.caramel.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Main) (source_name Caramel.Main))
       (Ir_letrec
         ((Exported ((path ()) (unique_name say_bye_4) (source_name say_bye))
            (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
              (Ir_lit (Lit_atom goodbye)))))
         (Ir_letrec
           ((Exported ((path ()) (unique_name say_hi_6) (source_name say_hi))
              (Ir_fun (((path ()) (unique_name param_7) (source_name param)))
                (Ir_lit (Lit_atom hello)))))
           (Ir_tuple
             ((Ir_var
                ((path ()) (unique_name say_bye_4) (source_name say_bye)))
               (Ir_var ((path ()) (unique_name say_hi_6) (source_name say_hi))))))))))

  $ cat main.caramel.b_0
  (Module (name Caramel.Main)
    (defs
      (((df_name (module_info 0))
         (df_body
           (Fun
             ((args ())
               (body
                 (Call (mod_ erlang) (fun_ get_module_info)
                   (args ((Literal (Lit_atom Caramel.Main))))))))))
        ((df_name (module_info 1))
          (df_body
            (Fun
              ((args (Opts))
                (body
                  (Call (mod_ erlang) (fun_ get_module_info)
                    (args ((Literal (Lit_atom Caramel.Main)) (Var Opts)))))))))
        ((df_name (say_hi 1))
          (df_body (Fun ((args (param)) (body (Literal (Lit_atom hello)))))))
        ((df_name (say_bye 1))
          (df_body (Fun ((args (param)) (body (Literal (Lit_atom goodbye)))))))))
    (exports ((say_hi 1) (say_bye 1) (module_info 0) (module_info 1))))

  $ cat Caramel.Main.core
  % Source code generated with Caramel.
  module 'Caramel.Main'
  [
   'say_hi'/1,
   'say_bye'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.Main') -| [])
  
  'module_info'/1 =
   (fun (Opts) -> call 'erlang':'get_module_info'('Caramel.Main', Opts) -| [])
  
  'say_hi'/1 = (fun (Param) -> 'hello' -| [])
  
  'say_bye'/1 = (fun (Param) -> 'goodbye' -| [])
  end
  

  $ erlc *.core
