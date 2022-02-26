  $ cat main.caramel
  external format: string -> list<'a> -> unit = "io:format"
  
  pub macro value(input) {
    match input {
    | Structured_input { value, _ } -> value
    }
  }
  
  pub macro print(input) {
    match input {
    | Structured_input { tag: :info, _} ->
      quote { format("INFO: ~p\n", [unquote(value(input))])}
    }
  }
  
  pub fn main(args) {
    print(Structured_input { tag: :info, value: "hello joe" })
  }

  $ caramel parse --file main.caramel --dump-tokens --debug
  caramel: [DEBUG] (External (Id format) Colon (Id string) Arrow (Id list)
                     Lesser_than (Type_var a) Greater_than Arrow (Id unit)
                     Equal (String io:format) Pub Macro (Id value) Parens_left
                     (Id input) Parens_right Brace_left Match (Id input)
                     Brace_left Pipe (Id Structured_input) Brace_left
                     (Id value) Comma Any Brace_right Arrow (Id value)
                     Brace_right Brace_right Pub Macro (Id print) Parens_left
                     (Id input) Parens_right Brace_left Match (Id input)
                     Brace_left Pipe (Id Structured_input) Brace_left (Id tag)
                     Colon (Atom info) Comma Any Brace_right Arrow Quote
                     Brace_left (Id format) Parens_left (String "INFO: ~p\\n")
                     Comma Bracket_left Unquote Parens_left (Id value)
                     Parens_left (Id input) Parens_right Parens_right
                     Bracket_right Parens_right Brace_right Brace_right
                     Brace_right Pub Fn (Id main) Parens_left (Id args)
                     Parens_right Brace_left (Id print) Parens_left
                     (Id Structured_input) Brace_left (Id tag) Colon
                     (Atom info) Comma (Id value) Colon (String "hello joe")
                     Brace_right Parens_right Brace_right)

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
                       ((fn_visibility Public) (fn_name (Id (value)))
                         (fn_args ((No_label (Pat_bind (Id (input))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_match (Expr_var (Id (input)))
                             (((cs_lhs
                                 (Pat_constructor (Id (Structured_input))
                                   (Ctp_record
                                     (((Id (value)) (Pat_bind (Id (value)))))
                                     Partial)))
                                (cs_rhs (Expr_var (Id (value))))))))
                         (fn_annot ())))
                     (Str_macro
                       ((fn_visibility Public) (fn_name (Id (print)))
                         (fn_args ((No_label (Pat_bind (Id (input))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_match (Expr_var (Id (input)))
                             (((cs_lhs
                                 (Pat_constructor (Id (Structured_input))
                                   (Ctp_record
                                     (((Id (tag))
                                        (Pat_literal (Lit_atom info))))
                                     Partial)))
                                (cs_rhs
                                  (Expr_quote
                                    ((Quasiquote
                                       ((Id format) Parens_left
                                         (String "INFO: ~p\\n") Comma
                                         Bracket_left))
                                      (Unquote
                                        (Expr_call (Expr_var (Id (value)))
                                          ((Expr_var (Id (input))))))
                                      (Quasiquote (Bracket_right Parens_right)))))))))
                         (fn_annot ())))
                     (Str_fun
                       ((fn_visibility Public) (fn_name (Id (main)))
                         (fn_args ((No_label (Pat_bind (Id (args))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_call (Expr_var (Id (print)))
                             ((Expr_constructor (Id (Structured_input))
                                (Ctr_record
                                  (((Id (tag)) (Expr_literal (Lit_atom info)))
                                    ((Id (value))
                                      (Expr_literal (Lit_string "hello joe")))))))))
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
                                           (fn_name (Id (value)))
                                           (fn_args
                                             ((No_label
                                                (Pat_bind (Id (input))))))
                                           (fn_arity 1)
                                           (fn_body
                                             (Expr_match
                                               (Expr_var (Id (input)))
                                               (((cs_lhs
                                                   (Pat_constructor
                                                     (Id (Structured_input))
                                                     (Ctp_record
                                                       (((Id (value))
                                                          (Pat_bind
                                                            (Id (value)))))
                                                       Partial)))
                                                  (cs_rhs
                                                    (Expr_var (Id (value))))))))
                                           (fn_annot ())))
                                       (Str_macro
                                         ((fn_visibility Public)
                                           (fn_name (Id (print)))
                                           (fn_args
                                             ((No_label
                                                (Pat_bind (Id (input))))))
                                           (fn_arity 1)
                                           (fn_body
                                             (Expr_match
                                               (Expr_var (Id (input)))
                                               (((cs_lhs
                                                   (Pat_constructor
                                                     (Id (Structured_input))
                                                     (Ctp_record
                                                       (((Id (tag))
                                                          (Pat_literal
                                                            (Lit_atom info))))
                                                       Partial)))
                                                  (cs_rhs
                                                    (Expr_quote
                                                      ((Quasiquote
                                                         ((Id format)
                                                           Parens_left
                                                           (String
                                                             "INFO: ~p\\n")
                                                           Comma Bracket_left))
                                                        (Unquote
                                                          (Expr_call
                                                            (Expr_var
                                                              (Id (value)))
                                                            ((Expr_var
                                                               (Id (input))))))
                                                        (Quasiquote
                                                          (Bracket_right
                                                            Parens_right)))))))))
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
                                                  (Lit_string "INFO: ~p\\n"))
                                                 (Expr_cons
                                                   (Expr_literal
                                                     (Lit_string "hello joe"))
                                                   Expr_nil))))
                                           (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-caml --debug
  caramel: [DEBUG] external format : string -> 'a list -> unit = "io:format"
                   let rec main args = format "INFO: ~p\\n" ["hello joe"]

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
    (main/4 (function args/5 (io:format "INFO: ~p\\n" [0: "hello joe" 0])))
    (makeblock 0 main/4))

  $ cat main.caramel.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Main) (source_name Caramel.Main))
       (Ir_letrec
         ((Exported ((path ()) (unique_name main_4) (source_name main))
            (Ir_fun (((path ()) (unique_name args_5) (source_name args)))
              (Ir_ext_call (io format)
                ((Ir_lit (Lit_string "INFO: ~p\\n"))
                  (Ir_cons (Ir_lit (Lit_string "hello joe")) Ir_nil))))))
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
                      ((Binary "INFO: ~p\\n")
                        (List (Binary "hello joe") (Literal Lit_nil))))))))))))
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
         #<73>(8,1,'integer',['unsigned'|['big']]),
         #<78>(8,1,'integer',['unsigned'|['big']]),
         #<70>(8,1,'integer',['unsigned'|['big']]),
         #<79>(8,1,'integer',['unsigned'|['big']]),
         #<58>(8,1,'integer',['unsigned'|['big']]),
         #<32>(8,1,'integer',['unsigned'|['big']]),
         #<126>(8,1,'integer',['unsigned'|['big']]),
         #<112>(8,1,'integer',['unsigned'|['big']]),
         #<10>(8,1,'integer',['unsigned'|['big']])
       }#,
       [
        #{
          #<104>(8,1,'integer',['unsigned'|['big']]),
          #<101>(8,1,'integer',['unsigned'|['big']]),
          #<108>(8,1,'integer',['unsigned'|['big']]),
          #<108>(8,1,'integer',['unsigned'|['big']]),
          #<111>(8,1,'integer',['unsigned'|['big']]),
          #<32>(8,1,'integer',['unsigned'|['big']]),
          #<106>(8,1,'integer',['unsigned'|['big']]),
          #<111>(8,1,'integer',['unsigned'|['big']]),
          #<101>(8,1,'integer',['unsigned'|['big']])
        }#|[] ]) -| [])
  end
  

  $ erlc *.core

  $ escript Caramel.Main.beam
  INFO: <<"hello joe">>
