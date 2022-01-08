  $ cat main.caramel
  external format: string -> list<'a> -> unit = "io:format"
  
  pub fn main(args) {
    match args {
    | [] -> format("~p\n", [:bye_bye])
    | [x, ...rest] ->
        format("Hello, ~s!\n", [ x ]);
        main(rest)
    }
  }

  $ caramel parse --file main.caramel --dump-tokens --debug
  caramel: [DEBUG] (External (Id format) Colon (Id string) Arrow (Id list)
                     Lesser_than (Type_var a) Greater_than Arrow (Id unit)
                     Equal (String io:format) Pub Fn (Id main) Parens_left
                     (Id args) Parens_right Brace_left Match (Id args)
                     Brace_left Pipe Bracket_left Bracket_right Arrow
                     (Id format) Parens_left (String "~p\\n") Comma
                     Bracket_left (Atom bye_bye) Bracket_right Parens_right
                     Pipe Bracket_left (Id x) Comma Dot_dot_dot (Id rest)
                     Bracket_right Arrow (Id format) Parens_left
                     (String "Hello, ~s!\\n") Comma Bracket_left (Id x)
                     Bracket_right Parens_right Semicolon (Id main) Parens_left
                     (Id rest) Parens_right Brace_right Brace_right)

  $ caramel parse --file main.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_extern
                      ((ext_name (Id format))
                        (ext_type
                          (Type_arrow (Type_name (Id string))
                            (Type_arrow (Type_apply (Id list) ((Type_var a)))
                              (Type_name (Id unit)))))
                        (ext_symbol io:format) (ext_visibility Private)
                        (ext_annot ())))
                     (Str_fun
                       ((fn_visibility Public) (fn_name (Id main))
                         (fn_args ((No_label (Pat_bind (Id args)))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_match (Expr_var (Id args))
                             (((cs_lhs Pat_nil)
                                (cs_rhs
                                  (Expr_call (Expr_var (Id format))
                                    ((Expr_literal (Lit_string "~p\\n"))
                                      (Expr_cons
                                        (Expr_literal (Lit_atom bye_bye))
                                        Expr_nil)))))
                               ((cs_lhs
                                  (Pat_cons (Pat_bind (Id x))
                                    (Pat_bind (Id rest))))
                                 (cs_rhs
                                   (Expr_seq
                                     (Expr_call (Expr_var (Id format))
                                       ((Expr_literal
                                          (Lit_string "Hello, ~s!\\n"))
                                         (Expr_cons (Expr_var (Id x)) Expr_nil)))
                                     (Expr_call (Expr_var (Id main))
                                       ((Expr_var (Id rest))))))))))
                         (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-caml --debug
  caramel: [DEBUG] external format : string -> 'a list -> unit = "io:format"
                   let rec main args =
                     match args with
                     | [] -> format "~p\\n" [`bye_bye]
                     | x::rest -> (format "Hello, ~s!\\n" [x]; main rest)

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
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
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
         (if args/5
           (seq (io:format "Hello, ~s!\\n" (makeblock 0 (field 0 args/5) 0))
             (apply main/4 (field 1 args/5)))
           (io:format "~p\\n" [0: 854901085 0]))))

  $ cat main.caramel.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Main) (source_name Caramel.Main))
       (Ir_letrec
         ((Exported ((path ()) (unique_name main_4) (source_name main))
            (Ir_fun (((path ()) (unique_name args_5) (source_name args)))
              (Ir_case
                (Ir_var ((path ()) (unique_name args_5) (source_name args)))
                ((P_nil
                   (Ir_ext_call (io format)
                     ((Ir_lit (Lit_string "~p\\n"))
                       (Ir_cons (Ir_lit (Lit_atom bye_bye)) Ir_nil))))
                  (P_ignore
                    (Ir_seq
                      (Ir_ext_call (io format)
                        ((Ir_lit (Lit_string "Hello, ~s!\\n"))
                          (Ir_cons
                            (Ir_ext_call (erlang hd)
                              ((Ir_var
                                 ((path ()) (unique_name args_5)
                                   (source_name args)))))
                            Ir_nil)))
                      (Ir_apply
                        (Ir_fn_name
                          ((path ()) (unique_name main_4) (source_name main))
                          1)
                        ((Ir_ext_call (erlang tl)
                           ((Ir_var
                              ((path ()) (unique_name args_5)
                                (source_name args))))))))))))))
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
                  (Case (cond (Var args))
                    (cases
                      ((Pat_nil
                         (Call (mod_ io) (fun_ format)
                           (args
                             ((Binary "~p\\n")
                               (List (Literal (Lit_atom bye_bye))
                                 (Literal Lit_nil))))))
                        (Pat_ignore
                          (Seq
                            (Call (mod_ io) (fun_ format)
                              (args
                                ((Binary "Hello, ~s!\\n")
                                  (List
                                    (Call (mod_ erlang) (fun_ hd)
                                      (args ((Var args))))
                                    (Literal Lit_nil)))))
                            (Apply (fn (Fun_ref (main 1)))
                              (args
                                ((Call (mod_ erlang) (fun_ tl)
                                   (args ((Var args)))))))))))))))))))
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
   
     case Args of
     <[]> when 'true' ->
       call 'io':'format'(
         #{
           #<126>(8,1,'integer',['unsigned'|['big']]),
           #<112>(8,1,'integer',['unsigned'|['big']]),
           #<10>(8,1,'integer',['unsigned'|['big']])
         }#, [ 'bye_bye'|[] ])
     <_> when 'true' ->
       do
         call 'io':'format'(
           #{
             #<72>(8,1,'integer',['unsigned'|['big']]),
             #<101>(8,1,'integer',['unsigned'|['big']]),
             #<108>(8,1,'integer',['unsigned'|['big']]),
             #<108>(8,1,'integer',['unsigned'|['big']]),
             #<111>(8,1,'integer',['unsigned'|['big']]),
             #<44>(8,1,'integer',['unsigned'|['big']]),
             #<32>(8,1,'integer',['unsigned'|['big']]),
             #<126>(8,1,'integer',['unsigned'|['big']]),
             #<115>(8,1,'integer',['unsigned'|['big']]),
             #<33>(8,1,'integer',['unsigned'|['big']]),
             #<10>(8,1,'integer',['unsigned'|['big']])
           }#, [ call 'erlang':'hd'(Args)|[] ])
          apply 'main'/1(call 'erlang':'tl'(Args))
     end -| [])
  end
  

  $ erlc *.core

  $ escript Caramel.Main.beam Joe Robert Mike
  Hello, Joe!
  Hello, Robert!
  Hello, Mike!
  bye_bye
