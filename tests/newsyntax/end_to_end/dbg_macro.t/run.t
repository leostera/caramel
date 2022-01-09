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

  $ caramel parse --file main.caramel --dump-tokens --debug
  caramel: [DEBUG] (External (Id format) Colon (Id string) Arrow (Id list)
                     Lesser_than (Type_var a) Greater_than Arrow (Id unit)
                     Equal (String io:format) Pub Macro (Id println)
                     Parens_left (Id val) Parens_right Brace_left Quote
                     Brace_left (Id format) Parens_left (String "~p\\n") Comma
                     Bracket_left (Id val) Bracket_right Parens_right
                     Brace_right Brace_right Pub Macro (Id dbg) Parens_left
                     (Id msg) Comma (Id val) Parens_right Brace_left
                     (Id println) Parens_left Parens_left Unquote Brace_left
                     (Id msg) Brace_right Comma Unquote Brace_left (Id val)
                     Brace_right Parens_right Parens_right Brace_right Pub Fn
                     (Id main) Parens_left (Id args) Parens_right Brace_left
                     (Id dbg) Parens_left (String input:) Comma Parens_left
                     (Atom args) Comma (Id args) Parens_right Parens_right
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
                       ((fn_visibility Public) (fn_name (Id (println)))
                         (fn_args ((No_label (Pat_bind (Id (val))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_quote
                             (Expr_call (Expr_var (Id (format)))
                               ((Expr_literal (Lit_string "~p\\n"))
                                 (Expr_cons (Expr_var (Id (val))) Expr_nil)))))
                         (fn_annot ())))
                     (Str_macro
                       ((fn_visibility Public) (fn_name (Id (dbg)))
                         (fn_args
                           ((No_label (Pat_bind (Id (msg))))
                             (No_label (Pat_bind (Id (val))))))
                         (fn_arity 2)
                         (fn_body
                           (Expr_call (Expr_var (Id (println)))
                             ((Expr_tuple
                                ((Expr_unquote (Expr_var (Id (msg))))
                                  (Expr_unquote (Expr_var (Id (val)))))))))
                         (fn_annot ())))
                     (Str_fun
                       ((fn_visibility Public) (fn_name (Id (main)))
                         (fn_args ((No_label (Pat_bind (Id (args))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_call (Expr_var (Id (dbg)))
                             ((Expr_literal (Lit_string input:))
                               (Expr_tuple
                                 ((Expr_literal (Lit_atom args))
                                   (Expr_var (Id (args))))))))
                         (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-caml --debug
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
  caramel: [DEBUG] external format : string -> 'a list -> unit = "io:format"
                   let rec main args =
                     format "~p\\n" [("input:", (`args, args))]

  $ caramel compile main.caramel --new-syntax --debug
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (main.caramel)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax true) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file main.caramel)
                                     (source_kind impl))
  
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
       (function args/5
         (io:format "~p\\n"
           (makeblock 0 (makeblock 0 "input:" (makeblock 0 -1066103459 args/5))
             0))))

  $ cat main.caramel.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Main) (source_name Caramel.Main))
       (Ir_letrec
         ((Exported ((path ()) (unique_name main_4) (source_name main))
            (Ir_fun (((path ()) (unique_name args_5) (source_name args)))
              (Ir_ext_call (io format)
                ((Ir_lit (Lit_string "~p\\n"))
                  (Ir_cons
                    (Ir_tuple
                      ((Ir_lit (Lit_string input:))
                        (Ir_tuple
                          ((Ir_lit (Lit_atom args))
                            (Ir_var
                              ((path ()) (unique_name args_5)
                                (source_name args)))))))
                    Ir_nil))))))
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
                      ((Binary "~p\\n")
                        (List
                          (Tuple
                            ((Binary input:)
                              (Tuple ((Literal (Lit_atom args)) (Var args)))))
                          (Literal Lit_nil))))))))))))
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
         #<10>(8,1,'integer',['unsigned'|['big']])
       }#,
       [
        {
         #{
           #<105>(8,1,'integer',['unsigned'|['big']]),
           #<110>(8,1,'integer',['unsigned'|['big']]),
           #<112>(8,1,'integer',['unsigned'|['big']]),
           #<117>(8,1,'integer',['unsigned'|['big']]),
           #<116>(8,1,'integer',['unsigned'|['big']]),
           #<58>(8,1,'integer',['unsigned'|['big']])
         }#, {'args', Args}}|[] ]) -| [])
  end
  

  $ erlc *.core

  $ escript Caramel.Main.beam Joe Robert Mike
  {<<"input:">>,{args,["Joe","Robert","Mike"]}}
