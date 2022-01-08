================================================================================

Verify that modules are properly renamed to include the namespacing of their
parents:

  $ caramel compile --debug --dump-pass 1 a.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (a.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass 1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file a.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing a.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing a.ml.ir_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.ir_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing a.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.B.C.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.B.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.B.C.D.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat a.ml.lambda
  (let
    (f/3 = (function param/5 : int 1)
     B/14 =
       (module-defn(B/14) A a.ml(3):14-144
         (let
           (f/6 = (function param/7 : int 1)
            C/13 =
              (module-defn(C/13) A.B a.ml(6):50-140
                (let
                  (f/8 = (function param/9 : int 1)
                   D/12 =
                     (module-defn(D/12) A.B.C a.ml(9):90-134
                       (let (f/10 = (function param/11 : int 1))
                         (makeblock 0 f/10))))
                  (makeblock 0 f/8 D/12))))
           (makeblock 0 f/6 C/13))))

  $ cat a.ml.ir_1
  (Ir_program
    ((Ir_module ((path ()) (unique_name Caramel.A) (source_name Caramel.A))
       (Ir_let Private ((path ()) (unique_name f_3) (source_name f))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_int 1)))
         (Ir_let Private ((path ()) (unique_name B_14) (source_name B))
           (Ir_module ((path (Caramel.A)) (unique_name B_14) (source_name B))
             (Ir_let Private ((path ()) (unique_name f_6) (source_name f))
               (Ir_fun (((path ()) (unique_name param_7) (source_name param)))
                 (Ir_lit (Lit_int 1)))
               (Ir_let Private ((path ()) (unique_name C_13) (source_name C))
                 (Ir_module
                   ((path (B Caramel.A)) (unique_name C_13) (source_name C))
                   (Ir_let Private
                     ((path ()) (unique_name f_8) (source_name f))
                     (Ir_fun
                       (((path ()) (unique_name param_9) (source_name param)))
                       (Ir_lit (Lit_int 1)))
                     (Ir_let Private
                       ((path ()) (unique_name D_12) (source_name D))
                       (Ir_module
                         ((path (C B Caramel.A)) (unique_name D_12)
                           (source_name D))
                         (Ir_let Private
                           ((path ()) (unique_name f_10) (source_name f))
                           (Ir_fun
                             (((path ()) (unique_name param_11)
                                (source_name param)))
                             (Ir_lit (Lit_int 1)))
                           (Ir_tuple
                             ((Ir_var
                                ((path ()) (unique_name f_10) (source_name f)))))))
                       (Ir_tuple
                         ((Ir_var
                            ((path ()) (unique_name f_8) (source_name f)))
                           (Ir_var
                             ((path ()) (unique_name D_12) (source_name D))))))))
                 (Ir_tuple
                   ((Ir_var ((path ()) (unique_name f_6) (source_name f)))
                     (Ir_var ((path ()) (unique_name C_13) (source_name C))))))))
           (Ir_tuple
             ((Ir_var ((path ()) (unique_name f_3) (source_name f)))
               (Ir_var ((path ()) (unique_name B_14) (source_name B))))))))))

  $ cat a.ml.b_0
  (Module (name Caramel.A.B.C)
    (defs
      (((df_name (module_info 0))
         (df_body
           (Fun
             ((args ())
               (body
                 (Call (mod_ erlang) (fun_ get_module_info)
                   (args ((Literal (Lit_atom Caramel.A.B.C))))))))))
        ((df_name (module_info 1))
          (df_body
            (Fun
              ((args (Opts))
                (body
                  (Call (mod_ erlang) (fun_ get_module_info)
                    (args ((Literal (Lit_atom Caramel.A.B.C)) (Var Opts)))))))))
        ((df_name (f 1))
          (df_body (Fun ((args (param)) (body (Literal (Lit_int 1)))))))))
    (exports ((f 1) (module_info 0) (module_info 1))))

  $ cat a.ml.b_1
  (Module (name Caramel.A.B)
    (defs
      (((df_name (module_info 0))
         (df_body
           (Fun
             ((args ())
               (body
                 (Call (mod_ erlang) (fun_ get_module_info)
                   (args ((Literal (Lit_atom Caramel.A.B))))))))))
        ((df_name (module_info 1))
          (df_body
            (Fun
              ((args (Opts))
                (body
                  (Call (mod_ erlang) (fun_ get_module_info)
                    (args ((Literal (Lit_atom Caramel.A.B)) (Var Opts)))))))))
        ((df_name (f 1))
          (df_body (Fun ((args (param)) (body (Literal (Lit_int 1)))))))))
    (exports ((f 1) (module_info 0) (module_info 1))))

  $ cat a.ml.b_2
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
        ((df_name (f 1))
          (df_body (Fun ((args (param)) (body (Literal (Lit_int 1)))))))))
    (exports ((f 1) (module_info 0) (module_info 1))))

  $ cat a.ml.b_3
  (Module (name Caramel.A.B.C.D)
    (defs
      (((df_name (module_info 0))
         (df_body
           (Fun
             ((args ())
               (body
                 (Call (mod_ erlang) (fun_ get_module_info)
                   (args ((Literal (Lit_atom Caramel.A.B.C.D))))))))))
        ((df_name (module_info 1))
          (df_body
            (Fun
              ((args (Opts))
                (body
                  (Call (mod_ erlang) (fun_ get_module_info)
                    (args ((Literal (Lit_atom Caramel.A.B.C.D)) (Var Opts)))))))))
        ((df_name (f 1))
          (df_body (Fun ((args (param)) (body (Literal (Lit_int 1)))))))))
    (exports ((f 1) (module_info 0) (module_info 1))))
