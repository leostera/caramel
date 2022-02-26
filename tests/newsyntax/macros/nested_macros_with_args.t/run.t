  $ cat main.caramel
  external format : string -> list<'a> -> unit = "io:format"
  
  pub macro format(tag, arg) {
    quote { format("~p: ~p", [ unquote(tag), unquote(arg) ]) }
  }
  
  pub macro reply(what) {
    format(:INFO, what)
  }
  
  pub macro hello(who) {
    reply(who)
  }
  
  pub fn main(args) {
    hello(:joe)
  }

  $ caramel parse --file main.caramel --dump-tokens --debug
  caramel: [DEBUG] (External (Id format) Colon (Id string) Arrow (Id list)
                     Lesser_than (Type_var a) Greater_than Arrow (Id unit)
                     Equal (String io:format) Pub Macro (Id format) Parens_left
                     (Id tag) Comma (Id arg) Parens_right Brace_left Quote
                     Brace_left (Id format) Parens_left (String "~p: ~p") Comma
                     Bracket_left Unquote Parens_left (Id tag) Parens_right
                     Comma Unquote Parens_left (Id arg) Parens_right
                     Bracket_right Parens_right Brace_right Brace_right Pub
                     Macro (Id reply) Parens_left (Id what) Parens_right
                     Brace_left (Id format) Parens_left (Atom INFO) Comma
                     (Id what) Parens_right Brace_right Pub Macro (Id hello)
                     Parens_left (Id who) Parens_right Brace_left (Id reply)
                     Parens_left (Id who) Parens_right Brace_right Pub Fn
                     (Id main) Parens_left (Id args) Parens_right Brace_left
                     (Id hello) Parens_left (Atom joe) Parens_right
                     Brace_right)

  $ caramel parse --file main.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_extern
                      ((ext_name (Id (format)))
                        (ext_type
                          (Type_arrow (Type_name (Id (string)))
                            (Type_arrow (Type_apply (Id (list)) ((Type_var a)))
                              (Type_name (Id (unit))))))
                        (ext_symbol io:format) (ext_visibility Private)
                        (ext_annot ())))
                     (Str_macro
                       ((fn_visibility Public) (fn_name (Id (format)))
                         (fn_args
                           ((No_label (Pat_bind (Id (tag))))
                             (No_label (Pat_bind (Id (arg))))))
                         (fn_arity 2)
                         (fn_body
                           (Expr_quote
                             ((Quasiquote
                                ((Id format) Parens_left (String "~p: ~p")
                                  Comma Bracket_left))
                               (Unquote (Expr_var (Id (tag))))
                               (Quasiquote (Comma))
                               (Unquote (Expr_var (Id (arg))))
                               (Quasiquote (Bracket_right Parens_right)))))
                         (fn_annot ())))
                     (Str_macro
                       ((fn_visibility Public) (fn_name (Id (reply)))
                         (fn_args ((No_label (Pat_bind (Id (what))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_call (Expr_var (Id (format)))
                             ((Expr_literal (Lit_atom INFO))
                               (Expr_var (Id (what))))))
                         (fn_annot ())))
                     (Str_macro
                       ((fn_visibility Public) (fn_name (Id (hello)))
                         (fn_args ((No_label (Pat_bind (Id (who))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_call (Expr_var (Id (reply)))
                             ((Expr_var (Id (who))))))
                         (fn_annot ())))
                     (Str_fun
                       ((fn_visibility Public) (fn_name (Id (main)))
                         (fn_args ((No_label (Pat_bind (Id (args))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_call (Expr_var (Id (hello)))
                             ((Expr_literal (Lit_atom joe)))))
                         (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-expanded --dump-macro-env --debug
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
                                           (fn_name (Id (format)))
                                           (fn_args
                                             ((No_label (Pat_bind (Id (tag))))
                                               (No_label (Pat_bind (Id (arg))))))
                                           (fn_arity 2)
                                           (fn_body
                                             (Expr_quote
                                               ((Quasiquote
                                                  ((Id format) Parens_left
                                                    (String "~p: ~p") Comma
                                                    Bracket_left))
                                                 (Unquote
                                                   (Expr_var (Id (tag))))
                                                 (Quasiquote (Comma))
                                                 (Unquote
                                                   (Expr_var (Id (arg))))
                                                 (Quasiquote
                                                   (Bracket_right Parens_right)))))
                                           (fn_annot ())))
                                       (Str_macro
                                         ((fn_visibility Public)
                                           (fn_name (Id (reply)))
                                           (fn_args
                                             ((No_label (Pat_bind (Id (what))))))
                                           (fn_arity 1)
                                           (fn_body
                                             (Expr_call
                                               (Expr_var (Id (format)))
                                               ((Expr_literal (Lit_atom INFO))
                                                 (Expr_var (Id (what))))))
                                           (fn_annot ())))
                                       (Str_macro
                                         ((fn_visibility Public)
                                           (fn_name (Id (hello)))
                                           (fn_args
                                             ((No_label (Pat_bind (Id (who))))))
                                           (fn_arity 1)
                                           (fn_body
                                             (Expr_call (Expr_var (Id (reply)))
                                               ((Expr_var (Id (who))))))
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
                                                  (Lit_string "~p: ~p"))
                                                 (Expr_cons
                                                   (Expr_literal
                                                     (Lit_atom INFO))
                                                   (Expr_cons
                                                     (Expr_literal
                                                       (Lit_atom joe))
                                                     Expr_nil)))))
                                           (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-caml --debug
  caramel: [DEBUG] external format : string -> 'a list -> unit = "io:format"
                   let rec main args = format "~p: ~p" [`INFO; `joe]

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
  (letrec
    (main/4
       (function args/5 (io:format "~p: ~p" [0: 813432942 [0: 5296128 0]])))
    (makeblock 0 main/4))

  $ cat main.caramel.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Main) (source_name Caramel.Main))
       (Ir_letrec
         ((Exported ((path ()) (unique_name main_4) (source_name main))
            (Ir_fun (((path ()) (unique_name args_5) (source_name args)))
              (Ir_ext_call (io format)
                ((Ir_lit (Lit_string "~p: ~p"))
                  (Ir_cons (Ir_lit (Lit_atom INFO))
                    (Ir_cons (Ir_lit (Lit_atom joe)) Ir_nil)))))))
         (Ir_tuple
           ((Ir_var ((path ()) (unique_name main_4) (source_name main)))))))))

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
                  (Call (mod_ io) (fun_ format)
                    (args
                      ((Binary "~p: ~p")
                        (List (Literal (Lit_atom INFO))
                          (List (Literal (Lit_atom joe)) (Literal Lit_nil)))))))))))))
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
  
  'main'/1 =
   (fun (Args) ->
   
     call 'io':'format'(
       #{
         #<126>(8,1,'integer',['unsigned'|['big']]),
         #<112>(8,1,'integer',['unsigned'|['big']]),
         #<58>(8,1,'integer',['unsigned'|['big']]),
         #<32>(8,1,'integer',['unsigned'|['big']]),
         #<126>(8,1,'integer',['unsigned'|['big']]),
         #<112>(8,1,'integer',['unsigned'|['big']])
       }#, [ 'INFO'|[ 'joe'|[] ] ]) -| [])
  end
  

  $ erlc *.core

  $ escript Caramel.Main.beam
  'INFO': joe
