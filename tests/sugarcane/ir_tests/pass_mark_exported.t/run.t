================================================================================

When there are no exports in the OCaml interface, there should be no exports in
the Core module either.

  $ caramel compile --debug --dump-pass=3 no_exports.mli no_exports.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (no_exports.mli no_exports.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass 3) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file no_exports.mli)
                                     (source_kind intf))
  
  caramel: [DEBUG] Writing no_exports.mli.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing no_exports.mli.typedtree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done
  caramel: [DEBUG] Compiling unit: ((source_file no_exports.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing no_exports.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing no_exports.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing no_exports.ml.ir_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing no_exports.ml.ir_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing no_exports.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing no_exports.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.No_exports.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat no_exports.mli

  $ cat no_exports.ml
  let f0 () = 1

  $ cat no_exports.mli.typedtree

  $ cat no_exports.ml.lambda

  $ cat no_exports.ml.ir_3
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.No_exports)
         (source_name Caramel.No_exports))
       (Ir_let Private ((path ()) (unique_name f0_3) (source_name f0))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_int 1)))
         (Ir_tuple ())))))

  $ cat no_exports.ml.b_0
  (Module (name Caramel.No_exports)
    (defs
      (((df_name (module_info 0))
         (df_body
           (Fun
             ((args ())
               (body
                 (Call (mod_ erlang) (fun_ get_module_info)
                   (args ((Literal (Lit_atom Caramel.No_exports))))))))))
        ((df_name (module_info 1))
          (df_body
            (Fun
              ((args (Opts))
                (body
                  (Call (mod_ erlang) (fun_ get_module_info)
                    (args ((Literal (Lit_atom Caramel.No_exports)) (Var Opts)))))))))
        ((df_name (f0 1))
          (df_body (Fun ((args (param)) (body (Literal (Lit_int 1)))))))))
    (exports ((module_info 0) (module_info 1))))

  $ cat Caramel.No_exports.core
  % Source code generated with Caramel.
  module 'Caramel.No_exports'
  [
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.No_exports') -| [])
  
  'module_info'/1 =
   (fun (Opts) ->
   call 'erlang':'get_module_info'('Caramel.No_exports', Opts) -| [])
  
  'f0'/1 = (fun (Param) -> 1 -| [])
  end
  

================================================================================

When there are some exports in the OCaml interface, there should be some
exports in the Core module either.

  $ caramel compile --debug --dump-pass=3 some_exports.mli some_exports.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (some_exports.mli some_exports.ml)) (stdlib (./))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass 3)
    (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file some_exports.mli)
                                     (source_kind intf))
  
  caramel: [DEBUG] Writing some_exports.mli.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing some_exports.mli.typedtree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done
  caramel: [DEBUG] Compiling unit: ((source_file some_exports.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing some_exports.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing some_exports.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing some_exports.ml.ir_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing some_exports.ml.ir_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing some_exports.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing some_exports.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Some_exports.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat some_exports.mli
  val f0 : unit -> int

  $ cat some_exports.ml
  let f0 () = 1
  let f1 () = 2

  $ cat some_exports.mli.typedtree
  [
    signature_item (some_exports.mli[1,0+0]..some_exports.mli[1,0+20])
      Tsig_value
      value_description f0/3 (some_exports.mli[1,0+0]..some_exports.mli[1,0+20])
        core_type (some_exports.mli[1,0+9]..some_exports.mli[1,0+20])
          Ttyp_arrow
          Nolabel
          core_type (some_exports.mli[1,0+9]..some_exports.mli[1,0+13])
            Ttyp_constr "unit/6!"
            []
          core_type (some_exports.mli[1,0+17]..some_exports.mli[1,0+20])
            Ttyp_constr "int/1!"
            []
        []
  ]

  $ cat some_exports.ml.lambda
  (let (f0/3 = (function param/5 : int 1) f1/6 = (function param/8 : int 2))
    (makeblock 0 f0/3))

  $ cat some_exports.ml.ir_3
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Some_exports)
         (source_name Caramel.Some_exports))
       (Ir_let Exported ((path ()) (unique_name f0_3) (source_name f0))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_int 1)))
         (Ir_let Private ((path ()) (unique_name f1_6) (source_name f1))
           (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
             (Ir_lit (Lit_int 2)))
           (Ir_tuple
             ((Ir_var ((path ()) (unique_name f0_3) (source_name f0))))))))))

  $ cat some_exports.ml.b_0
  (Module (name Caramel.Some_exports)
    (defs
      (((df_name (module_info 0))
         (df_body
           (Fun
             ((args ())
               (body
                 (Call (mod_ erlang) (fun_ get_module_info)
                   (args ((Literal (Lit_atom Caramel.Some_exports))))))))))
        ((df_name (module_info 1))
          (df_body
            (Fun
              ((args (Opts))
                (body
                  (Call (mod_ erlang) (fun_ get_module_info)
                    (args
                      ((Literal (Lit_atom Caramel.Some_exports)) (Var Opts)))))))))
        ((df_name (f1 1))
          (df_body (Fun ((args (param)) (body (Literal (Lit_int 2)))))))
        ((df_name (f0 1))
          (df_body (Fun ((args (param)) (body (Literal (Lit_int 1)))))))))
    (exports ((f0 1) (module_info 0) (module_info 1))))

  $ cat Caramel.Some_exports.core
  % Source code generated with Caramel.
  module 'Caramel.Some_exports'
  [
   'f0'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.Some_exports') -| [])
  
  'module_info'/1 =
   (fun (Opts) ->
   call 'erlang':'get_module_info'('Caramel.Some_exports', Opts) -| [])
  
  'f1'/1 = (fun (Param) -> 2 -| [])
  
  'f0'/1 = (fun (Param) -> 1 -| [])
  end
  

================================================================================

When there is no interface, everything should be exported in the Core module.

  $ caramel compile --debug --dump-pass=3 all_exports.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (all_exports.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass 3) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file all_exports.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing all_exports.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing all_exports.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing all_exports.ml.ir_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing all_exports.ml.ir_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing all_exports.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing all_exports.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.All_exports.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat all_exports.ml
  let f0 () = 1

  $ cat all_exports.ml.lambda

  $ cat all_exports.ml.ir_3
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.All_exports)
         (source_name Caramel.All_exports))
       (Ir_let Exported ((path ()) (unique_name f0_3) (source_name f0))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_int 1)))
         (Ir_tuple ((Ir_var ((path ()) (unique_name f0_3) (source_name f0)))))))))

  $ cat all_exports.ml.b_0
  (Module (name Caramel.All_exports)
    (defs
      (((df_name (module_info 0))
         (df_body
           (Fun
             ((args ())
               (body
                 (Call (mod_ erlang) (fun_ get_module_info)
                   (args ((Literal (Lit_atom Caramel.All_exports))))))))))
        ((df_name (module_info 1))
          (df_body
            (Fun
              ((args (Opts))
                (body
                  (Call (mod_ erlang) (fun_ get_module_info)
                    (args
                      ((Literal (Lit_atom Caramel.All_exports)) (Var Opts)))))))))
        ((df_name (f0 1))
          (df_body (Fun ((args (param)) (body (Literal (Lit_int 1)))))))))
    (exports ((f0 1) (module_info 0) (module_info 1))))

  $ cat Caramel.All_exports.core
  % Source code generated with Caramel.
  module 'Caramel.All_exports'
  [
   'f0'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.All_exports') -| [])
  
  'module_info'/1 =
   (fun (Opts) ->
   call 'erlang':'get_module_info'('Caramel.All_exports', Opts) -| [])
  
  'f0'/1 = (fun (Param) -> 1 -| [])
  end
  

