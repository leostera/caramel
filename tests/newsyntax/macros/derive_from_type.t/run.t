  $ cat main.caramel
  pub macro debug(ast) {
    quote {
      pub fn type_name() {
        unquote(ast.name)
      }
    }
  }
  
  @derive(debug)
  type t

  $ caramel parse --file main.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Macro (Id debug) Parens_left (Id ast) Parens_right
                     Brace_left Quote Brace_left Pub Fn (Id type_name)
                     Parens_left Parens_right Brace_left Unquote Parens_left
                     (Id ast.name) Parens_right Brace_right Brace_right
                     Brace_right At (Id derive) Parens_left (Id debug)
                     Parens_right Type (Id t))

  $ caramel parse --file main.caramel --dump-parsetree --debug
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
                       ((typ_name (Id (t))) (typ_args ())
                         (typ_desc Type_abstract)
                         (typ_annot
                           (((ann_name (Id (derive)))
                              (ann_desc ((Map (((Id (debug)) ())))))))))))

  $ caramel parse --file main.caramel --dump-expanded --dump-macro-env --debug
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
                                         ((typ_name (Id (t))) (typ_args ())
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
                                             (Expr_literal (Lit_string t)))
                                           (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-caml --debug
  caramel: [DEBUG] type t
                   let rec type_name () = "t"

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

  $ cat main.caramel.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Main) (source_name Caramel.Main))
       (Ir_letrec
         ((Exported
            ((path ()) (unique_name type_name_4) (source_name type_name))
            (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
              (Ir_lit (Lit_string t)))))
         (Ir_tuple
           ((Ir_var
              ((path ()) (unique_name type_name_4) (source_name type_name)))))))))

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
        ((df_name (type_name 1))
          (df_body (Fun ((args (param)) (body (Binary t))))))))
    (exports ((type_name 1) (module_info 0) (module_info 1))))

  $ cat Caramel.Main.core
  % Source code generated with Caramel.
  module 'Caramel.Main'
  [
   'type_name'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.Main') -| [])
  
  'module_info'/1 =
   (fun (Opts) -> call 'erlang':'get_module_info'('Caramel.Main', Opts) -| [])
  
  'type_name'/1 =
   (fun (Param) -> #{
                     #<116>(8,1,'integer',['unsigned'|['big']])
                   }# -| [])
  end
  

  $ erlc *.core
