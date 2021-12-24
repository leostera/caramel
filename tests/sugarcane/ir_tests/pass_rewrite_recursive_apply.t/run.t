================================================================================

Verify that calls to local functions go from (Apply (Var x)) to (Apply (Fn_name x arity)):

  $ caramel compile --debug --dump-pass 5 a.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (a.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass 5) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file a.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing a.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing a.ml.ir_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.ir_5
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing a.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat a.ml.lambda
  (let
    (f/3 = (function param/5 : int 1)
     g/6 = (function param/8 : int (apply f/3 0)))

  $ cat a.ml.ir_5
  (Ir_program
    ((Ir_module ((path ()) (unique_name Caramel.A) (source_name Caramel.A))
       (Ir_let Exported ((path ()) (unique_name f_3) (source_name f))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_int 1)))
         (Ir_let Exported ((path ()) (unique_name g_6) (source_name g))
           (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
             (Ir_apply
               (Ir_fn_name ((path ()) (unique_name f_3) (source_name f)) 1)
               ((Ir_lit (Lit_atom unit)))))
           (Ir_tuple
             ((Ir_var ((path ()) (unique_name f_3) (source_name f)))
               (Ir_var ((path ()) (unique_name g_6) (source_name g))))))))))

  $ cat a.ml.b_0
  (Module (name Caramel.A)
    (defs
      (((df_name (g 1))
         (df_body
           (Fun
             ((args (param))
               (body
                 (Apply (fn (Fun_ref (f 1)))
                   (args ((Literal (Lit_atom unit))))))))))
        ((df_name (f 1))
          (df_body (Fun ((args (param)) (body (Literal (Lit_int 1)))))))))
    (exports ((g 1) (f 1))))
