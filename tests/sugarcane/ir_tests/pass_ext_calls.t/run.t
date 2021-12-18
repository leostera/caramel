================================================================================

Verify that all qualified function calls end up as external calls:

  $ caramel compile --sugarcane --debug --dump-pass=2 a.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (a.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass 2)
    (dump_erl_ast true))
  
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
  caramel: [DEBUG] Writing a.ml.b_4
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.b_5
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing A.M1.S1.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing A.M1.S1.S2.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing A.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing A.M1.S1.S2.S3.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing A.M1.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing A.M0.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ bat a.ml
  (*** simple module ***)
  
  module M0 = struct
    let f () = 1
  end
  
  let c0 () = M0.f ()
  
  (*** nested module ***)
  
  module M1 = struct
    module S1 = struct
      module S2 = struct
        module S3 = struct
          let f () = 1
        end
      end
    end
  end
  
  let c1 () = M1.S1.S2.S3.f ()

  $ cat a.ml.ir_2
  (Ir_program
    ((Ir_module ((path ()) (unique_name A) (source_name A))
       (Ir_let ((path ()) (unique_name M0_6) (source_name M0))
         (Ir_module ((path (A)) (unique_name M0_6) (source_name M0))
           (Ir_let ((path ()) (unique_name f_3) (source_name f))
             (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
               (Ir_lit (Lit_int 1)))
             (Ir_record
               ((0 (Ir_var ((path ()) (unique_name f_3) (source_name f))))))))
         (Ir_let ((path ()) (unique_name c0_7) (source_name c0))
           (Ir_fun (((path ()) (unique_name param_9) (source_name param)))
             (Ir_ext_call (A.M0 f) ((Ir_lit (Lit_int 0)))))
           (Ir_let ((path ()) (unique_name M1_16) (source_name M1))
             (Ir_module ((path (A)) (unique_name M1_16) (source_name M1))
               (Ir_let ((path ()) (unique_name S1_15) (source_name S1))
                 (Ir_module
                   ((path (M1 A)) (unique_name S1_15) (source_name S1))
                   (Ir_let ((path ()) (unique_name S2_14) (source_name S2))
                     (Ir_module
                       ((path (S1 M1 A)) (unique_name S2_14) (source_name S2))
                       (Ir_let ((path ()) (unique_name S3_13) (source_name S3))
                         (Ir_module
                           ((path (S2 S1 M1 A)) (unique_name S3_13)
                             (source_name S3))
                           (Ir_let
                             ((path ()) (unique_name f_10) (source_name f))
                             (Ir_fun
                               (((path ()) (unique_name param_12)
                                  (source_name param)))
                               (Ir_lit (Lit_int 1)))
                             (Ir_record
                               ((0
                                  (Ir_var
                                    ((path ()) (unique_name f_10)
                                      (source_name f))))))))
                         (Ir_record
                           ((0
                              (Ir_var
                                ((path ()) (unique_name S3_13)
                                  (source_name S3))))))))
                     (Ir_record
                       ((0
                          (Ir_var
                            ((path ()) (unique_name S2_14) (source_name S2))))))))
                 (Ir_record
                   ((0
                      (Ir_var ((path ()) (unique_name S1_15) (source_name S1))))))))
             (Ir_let ((path ()) (unique_name c1_17) (source_name c1))
               (Ir_fun (((path ()) (unique_name param_19) (source_name param)))
                 (Ir_ext_call (A.M1.S1.S2.S3 f) ((Ir_lit (Lit_int 0)))))
               (Ir_record
                 ((0 (Ir_var ((path ()) (unique_name M0_6) (source_name M0))))
                   (1 (Ir_var ((path ()) (unique_name c0_7) (source_name c0))))
                   (2
                     (Ir_var ((path ()) (unique_name M1_16) (source_name M1))))
                   (3
                     (Ir_var ((path ()) (unique_name c1_17) (source_name c1)))))))))))))
