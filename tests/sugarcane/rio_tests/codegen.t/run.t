================================================================================

Lift IR into B-lang.

  $ caramel compile --debug a.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (a.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true))
  
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
  caramel: [DEBUG] Writing A.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing A.M1.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing A.M0.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat a.ml.ir
  (Ir_program
    ((Ir_module ((path ()) (unique_name A) (source_name A))
       (Ir_let ((path ()) (unique_name f0_12) (source_name f0))
         (Ir_fun (((path ()) (unique_name param_14) (source_name param)))
           (Ir_record
             ((0
                (Ir_ext_call (A.M0 m0_f)
                  ((Ir_lit (Lit_int 1)) (Ir_lit (Lit_int 2)))))
               (1 (Ir_ext_call (A.M1 m1_f) ((Ir_lit (Lit_int 2))))))))
         (Ir_record
           ((0 (Ir_var ((path ()) (unique_name M0_7) (source_name M0))))
             (1 (Ir_var ((path ()) (unique_name M1_11) (source_name M1))))
             (2 (Ir_var ((path ()) (unique_name f0_12) (source_name f0))))))))
      (Ir_module ((path (A)) (unique_name M1_11) (source_name M1))
        (Ir_let ((path ()) (unique_name m1_f_8) (source_name m1_f))
          (Ir_fun (((path ()) (unique_name y_10) (source_name y)))
            (Ir_record
              ((0 (Ir_var ((path ()) (unique_name y_10) (source_name y))))
                (1
                  (Ir_record
                    ((0
                       (Ir_var ((path ()) (unique_name y_10) (source_name y))))
                      (1 (Ir_lit (Lit_int 0)))))))))
          (Ir_record
            ((0 (Ir_var ((path ()) (unique_name m1_f_8) (source_name m1_f))))))))
      (Ir_module ((path (A)) (unique_name M0_7) (source_name M0))
        (Ir_let ((path ()) (unique_name m0_f_3) (source_name m0_f))
          (Ir_fun
            (((path ()) (unique_name x_5) (source_name x))
              ((path ()) (unique_name y_6) (source_name y)))
            (Ir_record
              ((0 (Ir_var ((path ()) (unique_name x_5) (source_name x))))
                (1 (Ir_var ((path ()) (unique_name y_6) (source_name y)))))))
          (Ir_record
            ((0 (Ir_var ((path ()) (unique_name m0_f_3) (source_name m0_f))))))))))

  $ cat A.M0.core
  % Source code generated with Caramel.
  module 'A.M0' [] attributes []
  'm0_f'/1 = ( fun (_x,_y) -> ~{ 0=>(_x -| []),1=>(_y -| []) }~ -| [])end
  

  $ erlc A.M0.core

  $ cat A.M1.core
  % Source code generated with Caramel.
  module 'A.M1' [] attributes []
  'm1_f'/1 = ( fun (_y) -> ~{ 0=>(_y -| []),1=>~{ 0=>(_y -| []),1=>0 }~ }~ -| [])end
  

  $ erlc A.M1.core

  $ cat A.core
  % Source code generated with Caramel.
  module 'A' [] attributes []
  'f0'/1 = ( fun (_param) -> ~{ 0=>call 'A.M0':'m0_f'(1,2),1=>call 'A.M1':'m1_f'(2) }~ -| [])end
  

  $ erlc A.core

================================================================================

  $ caramel compile --debug b.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (b.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file b.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing b.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing b.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing b.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing b.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing B.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat b.ml.lambda

  $ cat b.ml.ir
  (Ir_program
    ((Ir_module ((path ()) (unique_name B) (source_name B))
       (Ir_let ((path ()) (unique_name f_3) (source_name f))
         (Ir_fun (((path ()) (unique_name x_5) (source_name x)))
           (Ir_case
             (Ir_ext_call (erlang =/=)
               ((Ir_var ((path ()) (unique_name x_5) (source_name x)))
                 (Ir_lit (Lit_int 1))))
             (((P_lit (Lit_int 1)) (Ir_lit (Lit_int 0)))
               ((P_lit (Lit_int 0)) (Ir_lit (Lit_int 1))))))
         (Ir_record
           ((0 (Ir_var ((path ()) (unique_name f_3) (source_name f))))))))))

  $ cat B.core
  % Source code generated with Caramel.
  module 'B' [] attributes []
  'f'/1 = ( fun (_x) -> case call 'erlang':'=/='((_x -| []),1) of
  < _ > when 'true' -> 0
  
  < _ > when 'true' -> 1
  end
   -| [])end
  

  $ erlc B.core
  no_file: Warning: this clause cannot match because a previous clause always matches
  no_file: Warning: use of operator '=/=' has no effect


================================================================================

Sample. To add a new test, copy and paste this above, and replace `_.ml` with
the right .ml file. Then remove the `#` that are preventing these commands from
running.

  $ # caramel compile --debug _.ml

  $ # cat _.ml.lambda

  $ # cat _.ml.ir


