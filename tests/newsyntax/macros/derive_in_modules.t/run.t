  $ cat main.caramel
  pub macro type_name(t) {
    quote {
      pub fn type_name() { unquote(t.name) }
    }
  }
  
  module Abstract {
    @derive(type_name)
    type a_type
  }
  
  module Record {
    @derive(type_name)
    type a_record = { hello: string }
  }
  
  module Variant {
    @derive(type_name)
    type a_string = | Hi
  }

  $ caramel parse --file main.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Macro (Id type_name) Parens_left (Id t) Parens_right
                     Brace_left Quote Brace_left Pub Fn (Id type_name)
                     Parens_left Parens_right Brace_left Unquote Parens_left
                     (Id t.name) Parens_right Brace_right Brace_right
                     Brace_right Module (Id Abstract) Brace_left At (Id derive)
                     Parens_left (Id type_name) Parens_right Type (Id a_type)
                     Brace_right Module (Id Record) Brace_left At (Id derive)
                     Parens_left (Id type_name) Parens_right Type (Id a_record)
                     Equal Brace_left (Id hello) Colon (Id string) Brace_right
                     Brace_right Module (Id Variant) Brace_left At (Id derive)
                     Parens_left (Id type_name) Parens_right Type (Id a_string)
                     Equal Pipe (Id Hi) Brace_right)

  $ caramel parse --file main.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_macro
                      ((fn_visibility Public) (fn_name (Id (type_name)))
                        (fn_args ((No_label (Pat_bind (Id (t)))))) (fn_arity 1)
                        (fn_body
                          (Expr_quote
                            ((Quasiquote
                               (Pub Fn (Id type_name) Parens_left Parens_right
                                 Brace_left))
                              (Unquote
                                (Expr_field (Expr_var (Id (t))) (Id (name))))
                              (Quasiquote (Brace_right)))))
                        (fn_annot ())))
                     (Str_mod_expr
                       (Mod_decl
                         ((mod_name (Id (Abstract)))
                           (mod_items
                             ((Str_type
                                ((typ_name (Id (a_type))) (typ_args ())
                                  (typ_desc Type_abstract)
                                  (typ_annot
                                    (((ann_name (Id (derive)))
                                       (ann_desc
                                         ((Map (((Id (type_name)) ()))))))))))))
                           (mod_visibility Private) (mod_annot ()))))
                     (Str_mod_expr
                       (Mod_decl
                         ((mod_name (Id (Record)))
                           (mod_items
                             ((Str_type
                                ((typ_name (Id (a_record))) (typ_args ())
                                  (typ_desc
                                    (Type_record
                                      (tyk_labels
                                        (((lbl_name (Id (hello)))
                                           (lbl_type (Type_name (Id (string))))
                                           (lbl_annot ()))))))
                                  (typ_annot
                                    (((ann_name (Id (derive)))
                                       (ann_desc
                                         ((Map (((Id (type_name)) ()))))))))))))
                           (mod_visibility Private) (mod_annot ()))))
                     (Str_mod_expr
                       (Mod_decl
                         ((mod_name (Id (Variant)))
                           (mod_items
                             ((Str_type
                                ((typ_name (Id (a_string))) (typ_args ())
                                  (typ_desc
                                    (Type_variant
                                      (tyk_constructors
                                        (((ctr_name (Id (Hi)))
                                           (ctr_args (Tuple ()))
                                           (ctr_annot ()))))))
                                  (typ_annot
                                    (((ann_name (Id (derive)))
                                       (ann_desc
                                         ((Map (((Id (type_name)) ()))))))))))))
                           (mod_visibility Private) (mod_annot ())))))

  $ caramel parse --file main.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Public)
                                          (fn_name (Id (type_name)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (t))))))
                                          (fn_arity 1)
                                          (fn_body
                                            (Expr_quote
                                              ((Quasiquote
                                                 (Pub Fn (Id type_name)
                                                   Parens_left Parens_right
                                                   Brace_left))
                                                (Unquote
                                                  (Expr_field
                                                    (Expr_var (Id (t)))
                                                    (Id (name))))
                                                (Quasiquote (Brace_right)))))
                                          (fn_annot ())))
                                       (Str_mod_expr
                                         (Mod_decl
                                           ((mod_name (Id (Abstract)))
                                             (mod_items
                                               ((Str_type
                                                  ((typ_name (Id (a_type)))
                                                    (typ_args ())
                                                    (typ_desc Type_abstract)
                                                    (typ_annot
                                                      (((ann_name
                                                          (Id (derive)))
                                                         (ann_desc
                                                           ((Map
                                                              (((Id
                                                                  (type_name))
                                                                 ()))))))))))
                                                 (Str_fun
                                                   ((fn_visibility Public)
                                                     (fn_name (Id (type_name)))
                                                     (fn_args ()) (fn_arity 0)
                                                     (fn_body
                                                       (Expr_literal
                                                         (Lit_string a_type)))
                                                     (fn_annot ())))))
                                             (mod_visibility Private)
                                             (mod_annot ()))))
                                       (Str_mod_expr
                                         (Mod_decl
                                           ((mod_name (Id (Record)))
                                             (mod_items
                                               ((Str_type
                                                  ((typ_name (Id (a_record)))
                                                    (typ_args ())
                                                    (typ_desc
                                                      (Type_record
                                                        (tyk_labels
                                                          (((lbl_name
                                                              (Id (hello)))
                                                             (lbl_type
                                                               (Type_name
                                                                 (Id (string))))
                                                             (lbl_annot ()))))))
                                                    (typ_annot
                                                      (((ann_name
                                                          (Id (derive)))
                                                         (ann_desc
                                                           ((Map
                                                              (((Id
                                                                  (type_name))
                                                                 ()))))))))))
                                                 (Str_fun
                                                   ((fn_visibility Public)
                                                     (fn_name (Id (type_name)))
                                                     (fn_args ()) (fn_arity 0)
                                                     (fn_body
                                                       (Expr_literal
                                                         (Lit_string a_record)))
                                                     (fn_annot ())))))
                                             (mod_visibility Private)
                                             (mod_annot ()))))
                                       (Str_mod_expr
                                         (Mod_decl
                                           ((mod_name (Id (Variant)))
                                             (mod_items
                                               ((Str_type
                                                  ((typ_name (Id (a_string)))
                                                    (typ_args ())
                                                    (typ_desc
                                                      (Type_variant
                                                        (tyk_constructors
                                                          (((ctr_name
                                                              (Id (Hi)))
                                                             (ctr_args
                                                               (Tuple ()))
                                                             (ctr_annot ()))))))
                                                    (typ_annot
                                                      (((ann_name
                                                          (Id (derive)))
                                                         (ann_desc
                                                           ((Map
                                                              (((Id
                                                                  (type_name))
                                                                 ()))))))))))
                                                 (Str_fun
                                                   ((fn_visibility Public)
                                                     (fn_name (Id (type_name)))
                                                     (fn_args ()) (fn_arity 0)
                                                     (fn_body
                                                       (Expr_literal
                                                         (Lit_string a_string)))
                                                     (fn_annot ())))))
                                             (mod_visibility Private)
                                             (mod_annot ())))))

  $ caramel parse --file main.caramel --dump-caml --debug
  caramel: [DEBUG] module Abstract =
                     struct type a_type
                            let rec type_name () = "a_type" end
                   module Record =
                     struct
                       type a_record = {
                         hello: string }
                       let rec type_name () = "a_record"
                     end
                   module Variant =
                     struct
                       type a_string =
                         | Hi 
                       let rec type_name () = "a_string"
                     end

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
  caramel: [DEBUG] Writing main.caramel.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing main.caramel.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing main.caramel.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main.Record.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main.Variant.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main.Abstract.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat main.caramel.lambda
  (let
    (Abstract/6 =
       (module-defn(Abstract/6) <unknown location>
         (letrec (type_name/4 (function param/5 "a_type"))
           (makeblock 0 type_name/4)))
     Record/11 =
       (module-defn(Record/11) <unknown location>
         (letrec (type_name/9 (function param/10 "a_record"))
           (makeblock 0 type_name/9)))
     Variant/16 =
       (module-defn(Variant/16) <unknown location>
         (letrec (type_name/14 (function param/15 "a_string"))
           (makeblock 0 type_name/14))))
    (makeblock 0 Abstract/6 Record/11 Variant/16))

  $ cat main.caramel.ir
  (Ir_program
    ((Ir_module
       ((path (Caramel.Main)) (unique_name Record_11) (source_name Record))
       (Ir_letrec
         ((Exported
            ((path ()) (unique_name type_name_9) (source_name type_name))
            (Ir_fun (((path ()) (unique_name param_10) (source_name param)))
              (Ir_lit (Lit_string a_record)))))
         (Ir_tuple
           ((Ir_var
              ((path ()) (unique_name type_name_9) (source_name type_name)))))))
      (Ir_module
        ((path (Caramel.Main)) (unique_name Variant_16) (source_name Variant))
        (Ir_letrec
          ((Exported
             ((path ()) (unique_name type_name_14) (source_name type_name))
             (Ir_fun (((path ()) (unique_name param_15) (source_name param)))
               (Ir_lit (Lit_string a_string)))))
          (Ir_tuple
            ((Ir_var
               ((path ()) (unique_name type_name_14) (source_name type_name)))))))
      (Ir_module
        ((path ()) (unique_name Caramel.Main) (source_name Caramel.Main))
        (Ir_tuple
          ((Ir_var ((path ()) (unique_name Abstract_6) (source_name Abstract)))
            (Ir_var ((path ()) (unique_name Record_11) (source_name Record)))
            (Ir_var ((path ()) (unique_name Variant_16) (source_name Variant))))))
      (Ir_module
        ((path (Caramel.Main)) (unique_name Abstract_6) (source_name Abstract))
        (Ir_letrec
          ((Exported
             ((path ()) (unique_name type_name_4) (source_name type_name))
             (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
               (Ir_lit (Lit_string a_type)))))
          (Ir_tuple
            ((Ir_var
               ((path ()) (unique_name type_name_4) (source_name type_name)))))))))

  $ cat main.caramel.b_0
  (Module (name Caramel.Main.Record)
    (defs
      (((df_name (module_info 0))
         (df_body
           (Fun
             ((args ())
               (body
                 (Call (mod_ erlang) (fun_ get_module_info)
                   (args ((Literal (Lit_atom Caramel.Main.Record))))))))))
        ((df_name (module_info 1))
          (df_body
            (Fun
              ((args (Opts))
                (body
                  (Call (mod_ erlang) (fun_ get_module_info)
                    (args
                      ((Literal (Lit_atom Caramel.Main.Record)) (Var Opts)))))))))
        ((df_name (type_name 1))
          (df_body (Fun ((args (param)) (body (Binary a_record))))))))
    (exports ((type_name 1) (module_info 0) (module_info 1))))

  $ cat Caramel.*.core
  % Source code generated with Caramel.
  module 'Caramel.Main.Abstract'
  [
   'type_name'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.Main.Abstract') -| [])
  
  'module_info'/1 =
   (fun (Opts) ->
   call 'erlang':'get_module_info'('Caramel.Main.Abstract', Opts) -| [])
  
  'type_name'/1 =
   (fun (Param) ->
   
     #{
       #<97>(8,1,'integer',['unsigned'|['big']]),
       #<95>(8,1,'integer',['unsigned'|['big']]),
       #<116>(8,1,'integer',['unsigned'|['big']]),
       #<121>(8,1,'integer',['unsigned'|['big']]),
       #<112>(8,1,'integer',['unsigned'|['big']]),
       #<101>(8,1,'integer',['unsigned'|['big']])
     }# -| [])
  end
  
  % Source code generated with Caramel.
  module 'Caramel.Main.Record'
  [
   'type_name'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.Main.Record') -| [])
  
  'module_info'/1 =
   (fun (Opts) ->
   call 'erlang':'get_module_info'('Caramel.Main.Record', Opts) -| [])
  
  'type_name'/1 =
   (fun (Param) ->
   
     #{
       #<97>(8,1,'integer',['unsigned'|['big']]),
       #<95>(8,1,'integer',['unsigned'|['big']]),
       #<114>(8,1,'integer',['unsigned'|['big']]),
       #<101>(8,1,'integer',['unsigned'|['big']]),
       #<99>(8,1,'integer',['unsigned'|['big']]),
       #<111>(8,1,'integer',['unsigned'|['big']]),
       #<114>(8,1,'integer',['unsigned'|['big']]),
       #<100>(8,1,'integer',['unsigned'|['big']])
     }# -| [])
  end
  
  % Source code generated with Caramel.
  module 'Caramel.Main.Variant'
  [
   'type_name'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.Main.Variant') -| [])
  
  'module_info'/1 =
   (fun (Opts) ->
   call 'erlang':'get_module_info'('Caramel.Main.Variant', Opts) -| [])
  
  'type_name'/1 =
   (fun (Param) ->
   
     #{
       #<97>(8,1,'integer',['unsigned'|['big']]),
       #<95>(8,1,'integer',['unsigned'|['big']]),
       #<115>(8,1,'integer',['unsigned'|['big']]),
       #<116>(8,1,'integer',['unsigned'|['big']]),
       #<114>(8,1,'integer',['unsigned'|['big']]),
       #<105>(8,1,'integer',['unsigned'|['big']]),
       #<110>(8,1,'integer',['unsigned'|['big']]),
       #<103>(8,1,'integer',['unsigned'|['big']])
     }# -| [])
  end
  
  % Source code generated with Caramel.
  module 'Caramel.Main'
  [
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.Main') -| [])
  
  'module_info'/1 =
   (fun (Opts) -> call 'erlang':'get_module_info'('Caramel.Main', Opts) -| [])
  end
  

  $ erlc *.core
