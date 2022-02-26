  $ cat main.caramel
  pub macro debug(ast) {
    quote {
      pub fn type_name() {
        unquote(ast.name)
      }
    }
  }
  
  @derive(debug)
  type another_type<'a> = | Hello
  
  pub fn main(args) {
    type_name()
  }

  $ caramel parse --file main.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Macro (Id debug) Parens_left (Id ast) Parens_right
                     Brace_left Quote Brace_left Pub Fn (Id type_name)
                     Parens_left Parens_right Brace_left Unquote Parens_left
                     (Id ast.name) Parens_right Brace_right Brace_right
                     Brace_right At (Id derive) Parens_left (Id debug)
                     Parens_right Type (Id another_type) Lesser_than
                     (Type_var a) Greater_than Equal Pipe (Id Hello) Pub Fn
                     (Id main) Parens_left (Id args) Parens_right Brace_left
                     (Id type_name) Parens_left Parens_right Brace_right)

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
                       ((typ_name (Id (another_type))) (typ_args (a))
                         (typ_desc
                           (Type_variant
                             (tyk_constructors
                               (((ctr_name (Id (Hello))) (ctr_args (Tuple ()))
                                  (ctr_annot ()))))))
                         (typ_annot
                           (((ann_name (Id (derive)))
                              (ann_desc ((Map (((Id (debug)) ()))))))))))
                     (Str_fun
                       ((fn_visibility Public) (fn_name (Id (main)))
                         (fn_args ((No_label (Pat_bind (Id (args))))))
                         (fn_arity 1)
                         (fn_body (Expr_call (Expr_var (Id (type_name))) ()))
                         (fn_annot ()))))

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
                                         ((typ_name (Id (another_type)))
                                           (typ_args (a))
                                           (typ_desc
                                             (Type_variant
                                               (tyk_constructors
                                                 (((ctr_name (Id (Hello)))
                                                    (ctr_args (Tuple ()))
                                                    (ctr_annot ()))))))
                                           (typ_annot
                                             (((ann_name (Id (derive)))
                                                (ann_desc
                                                  ((Map (((Id (debug)) ()))))))))))
                                       (Str_fun
                                         ((fn_visibility Public)
                                           (fn_name (Id (type_name)))
                                           (fn_args ()) (fn_arity 0)
                                           (fn_body
                                             (Expr_literal
                                               (Lit_string another_type)))
                                           (fn_annot ())))
                                       (Str_fun
                                         ((fn_visibility Public)
                                           (fn_name (Id (main)))
                                           (fn_args
                                             ((No_label (Pat_bind (Id (args))))))
                                           (fn_arity 1)
                                           (fn_body
                                             (Expr_call
                                               (Expr_var (Id (type_name))) ()))
                                           (fn_annot ()))))
  $ caramel parse --file main.caramel --dump-caml --debug
  caramel: [DEBUG] type 'a another_type =
                     | Hello 
                   let rec type_name () = "another_type"
                   let rec main args = type_name ()

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
  (letrec (type_name/5 (function param/6 "another_type"))
    (letrec (main/7 (function args/8 (apply type_name/5 0)))
      (makeblock 0 type_name/5 main/7)))

  $ cat main.caramel.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Main) (source_name Caramel.Main))
       (Ir_letrec
         ((Exported
            ((path ()) (unique_name type_name_5) (source_name type_name))
            (Ir_fun (((path ()) (unique_name param_6) (source_name param)))
              (Ir_lit (Lit_string another_type)))))
         (Ir_letrec
           ((Exported ((path ()) (unique_name main_7) (source_name main))
              (Ir_fun (((path ()) (unique_name args_8) (source_name args)))
                (Ir_apply
                  (Ir_fn_name
                    ((path ()) (unique_name type_name_5)
                      (source_name type_name))
                    1)
                  ((Ir_lit (Lit_atom unit)))))))
           (Ir_tuple
             ((Ir_var
                ((path ()) (unique_name type_name_5) (source_name type_name)))
               (Ir_var ((path ()) (unique_name main_7) (source_name main))))))))))

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
        ((df_name (main 1))
          (df_body
            (Fun
              ((args (args))
                (body
                  (Apply (fn (Fun_ref (type_name 1)))
                    (args ((Literal (Lit_atom unit))))))))))
        ((df_name (type_name 1))
          (df_body (Fun ((args (param)) (body (Binary another_type))))))))
    (exports ((main 1) (type_name 1) (module_info 0) (module_info 1))))

  $ cat Caramel.Main.core
  % Source code generated with Caramel.
  module 'Caramel.Main'
  [
   'main'/1,
   'type_name'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.Main') -| [])
  
  'module_info'/1 =
   (fun (Opts) -> call 'erlang':'get_module_info'('Caramel.Main', Opts) -| [])
  
  'main'/1 = (fun (Args) -> apply 'type_name'/1('unit') -| [])
  
  'type_name'/1 =
   (fun (Param) ->
   
     #{
       #<97>(8,1,'integer',['unsigned'|['big']]),
       #<110>(8,1,'integer',['unsigned'|['big']]),
       #<111>(8,1,'integer',['unsigned'|['big']]),
       #<116>(8,1,'integer',['unsigned'|['big']]),
       #<104>(8,1,'integer',['unsigned'|['big']]),
       #<101>(8,1,'integer',['unsigned'|['big']]),
       #<114>(8,1,'integer',['unsigned'|['big']]),
       #<95>(8,1,'integer',['unsigned'|['big']]),
       #<116>(8,1,'integer',['unsigned'|['big']]),
       #<121>(8,1,'integer',['unsigned'|['big']]),
       #<112>(8,1,'integer',['unsigned'|['big']]),
       #<101>(8,1,'integer',['unsigned'|['big']])
     }# -| [])
  end
  

  $ erlc *.core

  $ escript Caramel.Main.beam Joe Robert Mike
