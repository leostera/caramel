================================================================================

  $ caramel compile --debug --dump-pass 2 a.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (a.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass 2) (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file a.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing a.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing a.ml.ir_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.ir_2
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
  caramel: [DEBUG] Writing A.M0.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing A.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing A.M1.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing A.M1.M0.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat a.ml.lambda
  (let
    (M0/9 =
       (module-defn(M0/9) Ml a.ml(3):25-84
         (let
           (m0_f/3 = (function param/5 : int 1)
            m0_g/6 = (function param/8 : int 2))
           (makeblock 0 m0_f/3 m0_g/6)))
     c0/10 = (function param/12 : int (apply (field 0 M0/9) 0))
     M1/20 =
       (module-defn(M1/20) Ml a.ml(13):135-223
         (let
           (M0/16 =
              (module-defn(M0/16) Ml.M1 a.ml(14):156-200
                (let (m0_f/13 = (function param/15 : int 1))
                  (makeblock 0 m0_f/13)))
            m1_f/17 = (function param/19 : int 2))
           (makeblock 0 M0/16 m1_f/17)))
     c1/21 = (function param/23 : int (apply (field 0 (field 0 M1/20)) 0)))
    (makeblock 0 M0/9 c0/10 M1/20 c1/21))

  $ cat a.ml.ir_0
  (Ir_program
    ((Ir_module ((path ()) (unique_name A) (source_name A))
       (Ir_let ((path ()) (unique_name M0_9) (source_name M0))
         (Ir_module ((path ()) (unique_name M0_9) (source_name M0))
           (Ir_let ((path ()) (unique_name m0_f_3) (source_name m0_f))
             (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
               (Ir_lit (Lit_int 1)))
             (Ir_let ((path ()) (unique_name m0_g_6) (source_name m0_g))
               (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
                 (Ir_lit (Lit_int 2)))
               (Ir_record
                 ((0
                    (Ir_var
                      ((path ()) (unique_name m0_f_3) (source_name m0_f))))
                   (1
                     (Ir_var
                       ((path ()) (unique_name m0_g_6) (source_name m0_g)))))))))
         (Ir_let ((path ()) (unique_name c0_10) (source_name c0))
           (Ir_fun (((path ()) (unique_name param_12) (source_name param)))
             (Ir_apply
               (Ir_field 0
                 (Ir_var ((path ()) (unique_name M0_9) (source_name M0))))
               ((Ir_lit (Lit_int 0)))))
           (Ir_let ((path ()) (unique_name M1_20) (source_name M1))
             (Ir_module ((path ()) (unique_name M1_20) (source_name M1))
               (Ir_let ((path ()) (unique_name M0_16) (source_name M0))
                 (Ir_module ((path ()) (unique_name M0_16) (source_name M0))
                   (Ir_let ((path ()) (unique_name m0_f_13) (source_name m0_f))
                     (Ir_fun
                       (((path ()) (unique_name param_15) (source_name param)))
                       (Ir_lit (Lit_int 1)))
                     (Ir_record
                       ((0
                          (Ir_var
                            ((path ()) (unique_name m0_f_13)
                              (source_name m0_f))))))))
                 (Ir_let ((path ()) (unique_name m1_f_17) (source_name m1_f))
                   (Ir_fun
                     (((path ()) (unique_name param_19) (source_name param)))
                     (Ir_lit (Lit_int 2)))
                   (Ir_record
                     ((0
                        (Ir_var
                          ((path ()) (unique_name M0_16) (source_name M0))))
                       (1
                         (Ir_var
                           ((path ()) (unique_name m1_f_17) (source_name m1_f)))))))))
             (Ir_let ((path ()) (unique_name c1_21) (source_name c1))
               (Ir_fun (((path ()) (unique_name param_23) (source_name param)))
                 (Ir_apply
                   (Ir_field 0
                     (Ir_field 0
                       (Ir_var
                         ((path ()) (unique_name M1_20) (source_name M1)))))
                   ((Ir_lit (Lit_int 0)))))
               (Ir_record
                 ((0 (Ir_var ((path ()) (unique_name M0_9) (source_name M0))))
                   (1
                     (Ir_var ((path ()) (unique_name c0_10) (source_name c0))))
                   (2
                     (Ir_var ((path ()) (unique_name M1_20) (source_name M1))))
                   (3
                     (Ir_var ((path ()) (unique_name c1_21) (source_name c1)))))))))))))

  $ cat a.ml.ir_2
  (Ir_program
    ((Ir_module ((path ()) (unique_name A) (source_name A))
       (Ir_let ((path ()) (unique_name M0_9) (source_name M0))
         (Ir_module ((path (A)) (unique_name M0_9) (source_name M0))
           (Ir_let ((path ()) (unique_name m0_f_3) (source_name m0_f))
             (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
               (Ir_lit (Lit_int 1)))
             (Ir_let ((path ()) (unique_name m0_g_6) (source_name m0_g))
               (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
                 (Ir_lit (Lit_int 2)))
               (Ir_record
                 ((0
                    (Ir_var
                      ((path ()) (unique_name m0_f_3) (source_name m0_f))))
                   (1
                     (Ir_var
                       ((path ()) (unique_name m0_g_6) (source_name m0_g)))))))))
         (Ir_let ((path ()) (unique_name c0_10) (source_name c0))
           (Ir_fun (((path ()) (unique_name param_12) (source_name param)))
             (Ir_ext_call (A.M0 m0_f) ((Ir_lit (Lit_int 0)))))
           (Ir_let ((path ()) (unique_name M1_20) (source_name M1))
             (Ir_module ((path (A)) (unique_name M1_20) (source_name M1))
               (Ir_let ((path ()) (unique_name M0_16) (source_name M0))
                 (Ir_module
                   ((path (M1 A)) (unique_name M0_16) (source_name M0))
                   (Ir_let ((path ()) (unique_name m0_f_13) (source_name m0_f))
                     (Ir_fun
                       (((path ()) (unique_name param_15) (source_name param)))
                       (Ir_lit (Lit_int 1)))
                     (Ir_record
                       ((0
                          (Ir_var
                            ((path ()) (unique_name m0_f_13)
                              (source_name m0_f))))))))
                 (Ir_let ((path ()) (unique_name m1_f_17) (source_name m1_f))
                   (Ir_fun
                     (((path ()) (unique_name param_19) (source_name param)))
                     (Ir_lit (Lit_int 2)))
                   (Ir_record
                     ((0
                        (Ir_var
                          ((path ()) (unique_name M0_16) (source_name M0))))
                       (1
                         (Ir_var
                           ((path ()) (unique_name m1_f_17) (source_name m1_f)))))))))
             (Ir_let ((path ()) (unique_name c1_21) (source_name c1))
               (Ir_fun (((path ()) (unique_name param_23) (source_name param)))
                 (Ir_ext_call (A.M1.M0 m0_f) ((Ir_lit (Lit_int 0)))))
               (Ir_record
                 ((0 (Ir_var ((path ()) (unique_name M0_9) (source_name M0))))
                   (1
                     (Ir_var ((path ()) (unique_name c0_10) (source_name c0))))
                   (2
                     (Ir_var ((path ()) (unique_name M1_20) (source_name M1))))
                   (3
                     (Ir_var ((path ()) (unique_name c1_21) (source_name c1)))))))))))))
