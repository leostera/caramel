  $ cat main.caramel
  pub macro pair(x) {
    quote { (unquote(x), unquote(x)) }
  }
  
  pub fn main(args) {
    pair(:hello)
  }

  $ caramel parse --file main.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Macro (Id pair) Parens_left (Id x) Parens_right
                     Brace_left Quote Brace_left Parens_left Unquote
                     Parens_left (Id x) Parens_right Comma Unquote Parens_left
                     (Id x) Parens_right Parens_right Brace_right Brace_right
                     Pub Fn (Id main) Parens_left (Id args) Parens_right
                     Brace_left (Id pair) Parens_left (Atom hello) Parens_right
                     Brace_right)

  $ caramel parse --file main.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_macro
                      ((fn_visibility Public) (fn_name (Id (pair)))
                        (fn_args ((No_label (Pat_bind (Id (x)))))) (fn_arity 1)
                        (fn_body
                          (Expr_quote
                            ((Quasiquote (Parens_left))
                              (Unquote (Expr_var (Id (x))))
                              (Quasiquote (Comma))
                              (Unquote (Expr_var (Id (x))))
                              (Quasiquote (Parens_right)))))
                        (fn_annot ())))
                     (Str_fun
                       ((fn_visibility Public) (fn_name (Id (main)))
                         (fn_args ((No_label (Pat_bind (Id (args))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_call (Expr_var (Id (pair)))
                             ((Expr_literal (Lit_atom hello)))))
                         (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Public)
                                          (fn_name (Id (pair)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (x))))))
                                          (fn_arity 1)
                                          (fn_body
                                            (Expr_quote
                                              ((Quasiquote (Parens_left))
                                                (Unquote (Expr_var (Id (x))))
                                                (Quasiquote (Comma))
                                                (Unquote (Expr_var (Id (x))))
                                                (Quasiquote (Parens_right)))))
                                          (fn_annot ())))
                                       (Str_fun
                                         ((fn_visibility Public)
                                           (fn_name (Id (main)))
                                           (fn_args
                                             ((No_label (Pat_bind (Id (args))))))
                                           (fn_arity 1)
                                           (fn_body
                                             (Expr_tuple
                                               ((Expr_literal (Lit_atom hello))
                                                 (Expr_literal
                                                   (Lit_atom hello)))))
                                           (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-caml --debug
  caramel: [DEBUG] let rec main args = (`hello, `hello)

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
  (letrec (main/3 (function args/4 [0: 616641298 616641298]))
    (makeblock 0 main/3))

  $ cat main.caramel.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Main) (source_name Caramel.Main))
       (Ir_letrec
         ((Exported ((path ()) (unique_name main_3) (source_name main))
            (Ir_fun (((path ()) (unique_name args_4) (source_name args)))
              (Ir_tuple ((Ir_lit (Lit_atom hello)) (Ir_lit (Lit_atom hello)))))))
         (Ir_tuple
           ((Ir_var ((path ()) (unique_name main_3) (source_name main)))))))))

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
                  (Tuple
                    ((Literal (Lit_atom hello)) (Literal (Lit_atom hello)))))))))))
    (exports ((main 1) (module_info 0) (module_info 1))))

  $ cat Caramel.Main.core
  % Source code generated with Caramel.
  module 'Caramel.Main'
  [
   'main'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.Main') -| [])
  
  'module_info'/1 =
   (fun (Opts) -> call 'erlang':'get_module_info'('Caramel.Main', Opts) -| [])
  
  'main'/1 = (fun (Args) -> {'hello', 'hello'} -| [])
  end
  

  $ erlc *.core

  $ escript Caramel.Main.beam
