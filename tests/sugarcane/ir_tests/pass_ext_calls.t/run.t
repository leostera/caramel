================================================================================

Verify that all qualified function calls end up as external calls:

  $ caramel compile --debug --dump-pass=3 a.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (a.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass 3) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file a.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing a.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing a.ml.ir_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing a.ml.ir_3
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
  caramel: [DEBUG] Writing Caramel.A.M1.S1.S2.S3.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.M1.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.M1.S1.S2.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.M0.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.A.M1.S1.core
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

  $ cat a.ml.ir_3
  (Ir_program
    ((Ir_module ((path ()) (unique_name Caramel.A) (source_name Caramel.A))
       (Ir_let Private ((path ()) (unique_name M0_6) (source_name M0))
         (Ir_module ((path (Caramel.A)) (unique_name M0_6) (source_name M0))
           (Ir_let Private ((path ()) (unique_name f_3) (source_name f))
             (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
               (Ir_lit (Lit_int 1)))
             (Ir_tuple
               ((Ir_var ((path ()) (unique_name f_3) (source_name f)))))))
         (Ir_let Private ((path ()) (unique_name c0_7) (source_name c0))
           (Ir_fun (((path ()) (unique_name param_9) (source_name param)))
             (Ir_ext_call (Caramel.A.M0 f) ((Ir_lit (Lit_atom unit)))))
           (Ir_let Private ((path ()) (unique_name M1_16) (source_name M1))
             (Ir_module
               ((path (Caramel.A)) (unique_name M1_16) (source_name M1))
               (Ir_let Private ((path ()) (unique_name S1_15) (source_name S1))
                 (Ir_module
                   ((path (M1 Caramel.A)) (unique_name S1_15) (source_name S1))
                   (Ir_let Private
                     ((path ()) (unique_name S2_14) (source_name S2))
                     (Ir_module
                       ((path (S1 M1 Caramel.A)) (unique_name S2_14)
                         (source_name S2))
                       (Ir_let Private
                         ((path ()) (unique_name S3_13) (source_name S3))
                         (Ir_module
                           ((path (S2 S1 M1 Caramel.A)) (unique_name S3_13)
                             (source_name S3))
                           (Ir_let Private
                             ((path ()) (unique_name f_10) (source_name f))
                             (Ir_fun
                               (((path ()) (unique_name param_12)
                                  (source_name param)))
                               (Ir_lit (Lit_int 1)))
                             (Ir_tuple
                               ((Ir_var
                                  ((path ()) (unique_name f_10)
                                    (source_name f)))))))
                         (Ir_tuple
                           ((Ir_var
                              ((path ()) (unique_name S3_13) (source_name S3)))))))
                     (Ir_tuple
                       ((Ir_var
                          ((path ()) (unique_name S2_14) (source_name S2)))))))
                 (Ir_tuple
                   ((Ir_var ((path ()) (unique_name S1_15) (source_name S1)))))))
             (Ir_let Private ((path ()) (unique_name c1_17) (source_name c1))
               (Ir_fun (((path ()) (unique_name param_19) (source_name param)))
                 (Ir_ext_call (Caramel.A.M1.S1.S2.S3 f)
                   ((Ir_lit (Lit_atom unit)))))
               (Ir_tuple
                 ((Ir_var ((path ()) (unique_name M0_6) (source_name M0)))
                   (Ir_var ((path ()) (unique_name c0_7) (source_name c0)))
                   (Ir_var ((path ()) (unique_name M1_16) (source_name M1)))
                   (Ir_var ((path ()) (unique_name c1_17) (source_name c1))))))))))))
