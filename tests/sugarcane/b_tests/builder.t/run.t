================================================================================

Lift IR into B-lang.

  $ caramel compile --debug a.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (a.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file a.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing a.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing a.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing a.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.M1.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.M0.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat a.ml.ir
  (Ir_program
    ((Ir_module ((path (Caramel.A)) (unique_name M1_10) (source_name M1))
       (Ir_let Exported ((path ()) (unique_name m1_f_7) (source_name m1_f))
         (Ir_fun (((path ()) (unique_name y_9) (source_name y)))
           (Ir_cons (Ir_var ((path ()) (unique_name y_9) (source_name y)))
             (Ir_cons (Ir_var ((path ()) (unique_name y_9) (source_name y)))
               Ir_nil)))
         (Ir_tuple
           ((Ir_var ((path ()) (unique_name m1_f_7) (source_name m1_f)))))))
      (Ir_module ((path ()) (unique_name Caramel.A) (source_name Caramel.A))
        (Ir_let Exported ((path ()) (unique_name f0_11) (source_name f0))
          (Ir_fun (((path ()) (unique_name param_13) (source_name param)))
            (Ir_tuple
              ((Ir_ext_call (Caramel.A.M0 m0_f) ((Ir_lit (Lit_int 1))))
                (Ir_ext_call (Caramel.A.M1 m1_f) ((Ir_lit (Lit_int 2)))))))
          (Ir_tuple
            ((Ir_var ((path ()) (unique_name M0_6) (source_name M0)))
              (Ir_var ((path ()) (unique_name M1_10) (source_name M1)))
              (Ir_var ((path ()) (unique_name f0_11) (source_name f0)))))))
      (Ir_module ((path (Caramel.A)) (unique_name M0_6) (source_name M0))
        (Ir_let Exported ((path ()) (unique_name m0_f_3) (source_name m0_f))
          (Ir_fun (((path ()) (unique_name x_5) (source_name x)))
            (Ir_tuple
              ((Ir_var ((path ()) (unique_name x_5) (source_name x)))
                (Ir_var ((path ()) (unique_name x_5) (source_name x))))))
          (Ir_tuple
            ((Ir_var ((path ()) (unique_name m0_f_3) (source_name m0_f)))))))))

  $ cat a.ml.b_0
  (Module (name Caramel.A.M1)
    (defs
      (((df_name (module_info 0))
         (df_body
           (Fun
             ((args ())
               (body
                 (Call (mod_ erlang) (fun_ get_module_info)
                   (args ((Literal (Lit_atom Caramel.A.M1))))))))))
        ((df_name (module_info 1))
          (df_body
            (Fun
              ((args (Opts))
                (body
                  (Call (mod_ erlang) (fun_ get_module_info)
                    (args ((Literal (Lit_atom Caramel.A.M1)) (Var Opts)))))))))
        ((df_name (m1_f 1))
          (df_body
            (Fun
              ((args (y))
                (body (List (Var y) (List (Var y) (Literal Lit_nil))))))))))
    (exports ((m1_f 1) (module_info 0) (module_info 1))))

  $ cat a.ml.b_1
  (Module (name Caramel.A)
    (defs
      (((df_name (module_info 0))
         (df_body
           (Fun
             ((args ())
               (body
                 (Call (mod_ erlang) (fun_ get_module_info)
                   (args ((Literal (Lit_atom Caramel.A))))))))))
        ((df_name (module_info 1))
          (df_body
            (Fun
              ((args (Opts))
                (body
                  (Call (mod_ erlang) (fun_ get_module_info)
                    (args ((Literal (Lit_atom Caramel.A)) (Var Opts)))))))))
        ((df_name (f0 1))
          (df_body
            (Fun
              ((args (param))
                (body
                  (Tuple
                    ((Call (mod_ Caramel.A.M0) (fun_ m0_f)
                       (args ((Literal (Lit_int 1)))))
                      (Call (mod_ Caramel.A.M1) (fun_ m1_f)
                        (args ((Literal (Lit_int 2))))))))))))))
    (exports ((f0 1) (module_info 0) (module_info 1))))

  $ cat a.ml.b_2
  (Module (name Caramel.A.M0)
    (defs
      (((df_name (module_info 0))
         (df_body
           (Fun
             ((args ())
               (body
                 (Call (mod_ erlang) (fun_ get_module_info)
                   (args ((Literal (Lit_atom Caramel.A.M0))))))))))
        ((df_name (module_info 1))
          (df_body
            (Fun
              ((args (Opts))
                (body
                  (Call (mod_ erlang) (fun_ get_module_info)
                    (args ((Literal (Lit_atom Caramel.A.M0)) (Var Opts)))))))))
        ((df_name (m0_f 1))
          (df_body (Fun ((args (x)) (body (Tuple ((Var x) (Var x))))))))))
    (exports ((m0_f 1) (module_info 0) (module_info 1))))


================================================================================

Sample. To add a new test, copy and paste this above, and replace `_.ml` with
the right .ml file. Then remove the `#` that are preventing these commands from
running.

  $ # caramel compile --sugarcane --debug _.ml

  $ # cat _.ml.lambda

  $ # cat _.ml.ir


