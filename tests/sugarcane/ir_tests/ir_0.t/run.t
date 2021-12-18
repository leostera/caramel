================================================================================

We expect to not be able to compile top-level module values that are not
functions:

  $ caramel compile --sugarcane --debug lambda_let_val.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_let_val.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_let_val.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_let_val.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_let_val.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  TODO: Support for this lambda construct has not been implemented yet: 
  (let (a/3 =[int] 1) (makeblock 0 a/3))
  [1]

  $ cat lambda_let_val.ml.lambda

  $ cat lambda_let_val.ml.ir
  cat: lambda_let_val.ml.ir: No such file or directory
  [1]

================================================================================

Top-level module functions, however, should be lifted to the IR:

  $ caramel compile --sugarcane --debug lambda_let_fun.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_let_fun.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_let_fun.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_let_fun.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_let_fun.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing lambda_let_fun.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_let_fun.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_let_fun.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat lambda_let_fun.ml.lambda

  $ cat lambda_let_fun.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Lambda_let_fun) (source_name Lambda_let_fun))
       (Ir_let ((path ()) (unique_name a_3) (source_name a))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_int 1)))
         (Ir_record
           ((0 (Ir_var ((path ()) (unique_name a_3) (source_name a))))))))))


================================================================================

Support for function application:

  $ caramel compile --sugarcane --debug lambda_apply.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_apply.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_apply.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_apply.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_apply.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing lambda_apply.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_apply.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_apply.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat lambda_apply.ml.lambda
  (let
    (a/3 = (function param/5 : int 1)
     x/6 = (function param/8 : int (apply a/3 0)))

  $ cat lambda_apply.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Lambda_apply) (source_name Lambda_apply))
       (Ir_let ((path ()) (unique_name a_3) (source_name a))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_int 1)))
         (Ir_let ((path ()) (unique_name x_6) (source_name x))
           (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
             (Ir_apply (Ir_var ((path ()) (unique_name a_3) (source_name a)))
               ((Ir_lit (Lit_int 0)))))
           (Ir_record
             ((0 (Ir_var ((path ()) (unique_name a_3) (source_name a))))
               (1 (Ir_var ((path ()) (unique_name x_6) (source_name x)))))))))))


================================================================================

Support for literals:

  $ caramel compile --sugarcane --debug lambda_const.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_const.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_const.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_const.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_const.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing lambda_const.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_const.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_const.core
  Binary
  TODO: This function has not been implemented yet: unsupported expression
  [1]

  $ cat lambda_const.ml.lambda
  (let
    (int/3 = (function param/5 : int 0)
     nativeint/6 = (function param/8 : nativeint 0n)
     int32/9 = (function param/11 : int32 0l)
     int64/12 = (function param/14 : int64 0L)
     float/15 = (function param/17 : float 0.0123412)
     char/18 = (function param/20 : int 'a')
     string/21 = (function param/23 "hello world")
     multiline_string/24 = (function param/26 "\nhello world\n\n")
     bool_true/27 = (function param/29 1)
     bool_false/30 = (function param/32 0))
    (makeblock 0 int/3 nativeint/6 int32/9 int64/12 float/15 char/18 string/21
      multiline_string/24 bool_true/27 bool_false/30))

  $ cat lambda_const.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Lambda_const) (source_name Lambda_const))
       (Ir_let ((path ()) (unique_name int_3) (source_name int))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_int 0)))
         (Ir_let ((path ()) (unique_name nativeint_6) (source_name nativeint))
           (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
             (Ir_lit (Lit_int 0)))
           (Ir_let ((path ()) (unique_name int32_9) (source_name int32))
             (Ir_fun (((path ()) (unique_name param_11) (source_name param)))
               (Ir_lit (Lit_int 0)))
             (Ir_let ((path ()) (unique_name int64_12) (source_name int64))
               (Ir_fun (((path ()) (unique_name param_14) (source_name param)))
                 (Ir_lit (Lit_int 0)))
               (Ir_let ((path ()) (unique_name float_15) (source_name float))
                 (Ir_fun
                   (((path ()) (unique_name param_17) (source_name param)))
                   (Ir_lit (Lit_float 0.0123412)))
                 (Ir_let ((path ()) (unique_name char_18) (source_name char))
                   (Ir_fun
                     (((path ()) (unique_name param_20) (source_name param)))
                     (Ir_lit (Lit_char a)))
                   (Ir_let
                     ((path ()) (unique_name string_21) (source_name string))
                     (Ir_fun
                       (((path ()) (unique_name param_23) (source_name param)))
                       (Ir_lit (Lit_string "hello world")))
                     (Ir_let
                       ((path ()) (unique_name multiline_string_24)
                         (source_name multiline_string))
                       (Ir_fun
                         (((path ()) (unique_name param_26)
                            (source_name param)))
                         (Ir_lit (Lit_string  "\
                                             \nhello world\
                                             \n\
                                             \n")))
                       (Ir_let
                         ((path ()) (unique_name bool_true_27)
                           (source_name bool_true))
                         (Ir_fun
                           (((path ()) (unique_name param_29)
                              (source_name param)))
                           (Ir_lit (Lit_int 1)))
                         (Ir_let
                           ((path ()) (unique_name bool_false_30)
                             (source_name bool_false))
                           (Ir_fun
                             (((path ()) (unique_name param_32)
                                (source_name param)))
                             (Ir_lit (Lit_int 0)))
                           (Ir_record
                             ((0
                                (Ir_var
                                  ((path ()) (unique_name int_3)
                                    (source_name int))))
                               (1
                                 (Ir_var
                                   ((path ()) (unique_name nativeint_6)
                                     (source_name nativeint))))
                               (2
                                 (Ir_var
                                   ((path ()) (unique_name int32_9)
                                     (source_name int32))))
                               (3
                                 (Ir_var
                                   ((path ()) (unique_name int64_12)
                                     (source_name int64))))
                               (4
                                 (Ir_var
                                   ((path ()) (unique_name float_15)
                                     (source_name float))))
                               (5
                                 (Ir_var
                                   ((path ()) (unique_name char_18)
                                     (source_name char))))
                               (6
                                 (Ir_var
                                   ((path ()) (unique_name string_21)
                                     (source_name string))))
                               (7
                                 (Ir_var
                                   ((path ()) (unique_name multiline_string_24)
                                     (source_name multiline_string))))
                               (8
                                 (Ir_var
                                   ((path ()) (unique_name bool_true_27)
                                     (source_name bool_true))))
                               (9
                                 (Ir_var
                                   ((path ()) (unique_name bool_false_30)
                                     (source_name bool_false)))))))))))))))))))


================================================================================

Functions and lambdas!

  $ caramel compile --sugarcane --debug lambda_fun.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_fun.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_fun.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_fun.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_fun.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing lambda_fun.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_fun.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_fun.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat lambda_fun.ml.lambda
  (let
    (f/3 =
       (function _arg0/5 _arg1/6
         (let (lambda/7 = (function x1/9 _x2/10 _x3/11 x1/9))
           (apply lambda/7 0)))
     g/12 = (function _arg0/14 (apply f/3 0)))

  $ cat lambda_fun.ml.ir
  (Ir_program
    ((Ir_module ((path ()) (unique_name Lambda_fun) (source_name Lambda_fun))
       (Ir_let ((path ()) (unique_name f_3) (source_name f))
         (Ir_fun
           (((path ()) (unique_name _arg0_5) (source_name _arg0))
             ((path ()) (unique_name _arg1_6) (source_name _arg1)))
           (Ir_let ((path ()) (unique_name lambda_7) (source_name lambda))
             (Ir_fun
               (((path ()) (unique_name x1_9) (source_name x1))
                 ((path ()) (unique_name _x2_10) (source_name _x2))
                 ((path ()) (unique_name _x3_11) (source_name _x3)))
               (Ir_var ((path ()) (unique_name x1_9) (source_name x1))))
             (Ir_apply
               (Ir_var ((path ()) (unique_name lambda_7) (source_name lambda)))
               ((Ir_lit (Lit_int 0))))))
         (Ir_let ((path ()) (unique_name g_12) (source_name g))
           (Ir_fun (((path ()) (unique_name _arg0_14) (source_name _arg0)))
             (Ir_apply (Ir_var ((path ()) (unique_name f_3) (source_name f)))
               ((Ir_lit (Lit_int 0)))))
           (Ir_record
             ((0 (Ir_var ((path ()) (unique_name f_3) (source_name f))))
               (1 (Ir_var ((path ()) (unique_name g_12) (source_name g)))))))))))


================================================================================

Support for primitives:

  $ caramel compile --sugarcane --debug lambda_prim.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_prim.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_prim.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_prim.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_prim.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing lambda_prim.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_prim.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_prim.core
  Binary
  TODO: This function has not been implemented yet: unsupported expression
  [1]

  $ cat lambda_prim.ml.lambda
  (let
    (unit/3 = (function param/5 0)
     tuple2/6 = (function param/8 [0: 1 2])
     tuple3/9 = (function param/11 [0: 1 2 3])
     tuple4/12 = (function param/14 [0: 1 2 3 4])
     tuple5/15 = (function param/17 [0: 1 2 3 4 5])
     polyvar0/18 = (function param/20 -822631612)
     polyvar1/21 = (function param/23 [0: -910739301 1])
     polyvar2/24 = (function param/26 [0: -910739301 [0: 1 1]])
     r0/36 = (function param/38 [0: 1])
     r1/39 = (function param/41 [0: 1 "record"])
     r2/42 = (function param/44 [0: 1 "record" 1])
     f0/45 = (function param/47 : int (field 0 [0: 1]))
     f1/48 = (function param/50 : int (field 0 (apply r0/36 0)))
     f2/51 = (function r/53 : int (field 0 r/53))
     f3/54 = (function param/57 (field 2 param/57))
     v0/64 = (function param/66 0)
     v1/67 = (function param/69 [0: 1])
     v2/70 = (function param/72 [1: 1 2])
     vr/73 = (function param/75 [2: 0])
     gadt/78 = (function param/80 [0: "what"]))
    (makeblock 0 unit/3 tuple2/6 tuple3/9 tuple4/12 tuple5/15 polyvar0/18
      polyvar1/21 polyvar2/24 r0/36 r1/39 r2/42 f0/45 f1/48 f2/51 f3/54 v0/64
      v1/67 v2/70 vr/73 gadt/78))

  $ cat lambda_prim.ml.ir
  (Ir_program
    ((Ir_module ((path ()) (unique_name Lambda_prim) (source_name Lambda_prim))
       (Ir_let ((path ()) (unique_name unit_3) (source_name unit))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_int 0)))
         (Ir_let ((path ()) (unique_name tuple2_6) (source_name tuple2))
           (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
             (Ir_record ((0 (Ir_lit (Lit_int 1))) (1 (Ir_lit (Lit_int 2))))))
           (Ir_let ((path ()) (unique_name tuple3_9) (source_name tuple3))
             (Ir_fun (((path ()) (unique_name param_11) (source_name param)))
               (Ir_record
                 ((0 (Ir_lit (Lit_int 1))) (1 (Ir_lit (Lit_int 2)))
                   (2 (Ir_lit (Lit_int 3))))))
             (Ir_let ((path ()) (unique_name tuple4_12) (source_name tuple4))
               (Ir_fun (((path ()) (unique_name param_14) (source_name param)))
                 (Ir_record
                   ((0 (Ir_lit (Lit_int 1))) (1 (Ir_lit (Lit_int 2)))
                     (2 (Ir_lit (Lit_int 3))) (3 (Ir_lit (Lit_int 4))))))
               (Ir_let ((path ()) (unique_name tuple5_15) (source_name tuple5))
                 (Ir_fun
                   (((path ()) (unique_name param_17) (source_name param)))
                   (Ir_record
                     ((0 (Ir_lit (Lit_int 1))) (1 (Ir_lit (Lit_int 2)))
                       (2 (Ir_lit (Lit_int 3))) (3 (Ir_lit (Lit_int 4)))
                       (4 (Ir_lit (Lit_int 5))))))
                 (Ir_let
                   ((path ()) (unique_name polyvar0_18) (source_name polyvar0))
                   (Ir_fun
                     (((path ()) (unique_name param_20) (source_name param)))
                     (Ir_lit (Lit_int -822631612)))
                   (Ir_let
                     ((path ()) (unique_name polyvar1_21)
                       (source_name polyvar1))
                     (Ir_fun
                       (((path ()) (unique_name param_23) (source_name param)))
                       (Ir_record
                         ((0 (Ir_lit (Lit_int -910739301)))
                           (1 (Ir_lit (Lit_int 1))))))
                     (Ir_let
                       ((path ()) (unique_name polyvar2_24)
                         (source_name polyvar2))
                       (Ir_fun
                         (((path ()) (unique_name param_26)
                            (source_name param)))
                         (Ir_record
                           ((0 (Ir_lit (Lit_int -910739301)))
                             (1
                               (Ir_record
                                 ((0 (Ir_lit (Lit_int 1)))
                                   (1 (Ir_lit (Lit_int 1)))))))))
                       (Ir_let ((path ()) (unique_name r0_36) (source_name r0))
                         (Ir_fun
                           (((path ()) (unique_name param_38)
                              (source_name param)))
                           (Ir_record ((0 (Ir_lit (Lit_int 1))))))
                         (Ir_let
                           ((path ()) (unique_name r1_39) (source_name r1))
                           (Ir_fun
                             (((path ()) (unique_name param_41)
                                (source_name param)))
                             (Ir_record
                               ((0 (Ir_lit (Lit_int 1)))
                                 (1 (Ir_lit (Lit_string record))))))
                           (Ir_let
                             ((path ()) (unique_name r2_42) (source_name r2))
                             (Ir_fun
                               (((path ()) (unique_name param_44)
                                  (source_name param)))
                               (Ir_record
                                 ((0 (Ir_lit (Lit_int 1)))
                                   (1 (Ir_lit (Lit_string record)))
                                   (2 (Ir_lit (Lit_int 1))))))
                             (Ir_let
                               ((path ()) (unique_name f0_45) (source_name f0))
                               (Ir_fun
                                 (((path ()) (unique_name param_47)
                                    (source_name param)))
                                 (Ir_field 0
                                   (Ir_record ((0 (Ir_lit (Lit_int 1)))))))
                               (Ir_let
                                 ((path ()) (unique_name f1_48)
                                   (source_name f1))
                                 (Ir_fun
                                   (((path ()) (unique_name param_50)
                                      (source_name param)))
                                   (Ir_field 0
                                     (Ir_apply
                                       (Ir_var
                                         ((path ()) (unique_name r0_36)
                                           (source_name r0)))
                                       ((Ir_lit (Lit_int 0))))))
                                 (Ir_let
                                   ((path ()) (unique_name f2_51)
                                     (source_name f2))
                                   (Ir_fun
                                     (((path ()) (unique_name r_53)
                                        (source_name r)))
                                     (Ir_field 0
                                       (Ir_var
                                         ((path ()) (unique_name r_53)
                                           (source_name r)))))
                                   (Ir_let
                                     ((path ()) (unique_name f3_54)
                                       (source_name f3))
                                     (Ir_fun
                                       (((path ()) (unique_name param_57)
                                          (source_name param)))
                                       (Ir_field 2
                                         (Ir_var
                                           ((path ()) (unique_name param_57)
                                             (source_name param)))))
                                     (Ir_let
                                       ((path ()) (unique_name v0_64)
                                         (source_name v0))
                                       (Ir_fun
                                         (((path ()) (unique_name param_66)
                                            (source_name param)))
                                         (Ir_lit (Lit_int 0)))
                                       (Ir_let
                                         ((path ()) (unique_name v1_67)
                                           (source_name v1))
                                         (Ir_fun
                                           (((path ()) (unique_name param_69)
                                              (source_name param)))
                                           (Ir_record
                                             ((0 (Ir_lit (Lit_int 1))))))
                                         (Ir_let
                                           ((path ()) (unique_name v2_70)
                                             (source_name v2))
                                           (Ir_fun
                                             (((path ()) (unique_name param_72)
                                                (source_name param)))
                                             (Ir_record
                                               ((0 (Ir_lit (Lit_int 1)))
                                                 (1 (Ir_lit (Lit_int 2))))))
                                           (Ir_let
                                             ((path ()) (unique_name vr_73)
                                               (source_name vr))
                                             (Ir_fun
                                               (((path ())
                                                  (unique_name param_75)
                                                  (source_name param)))
                                               (Ir_record
                                                 ((0 (Ir_lit (Lit_int 0))))))
                                             (Ir_let
                                               ((path ()) (unique_name gadt_78)
                                                 (source_name gadt))
                                               (Ir_fun
                                                 (((path ())
                                                    (unique_name param_80)
                                                    (source_name param)))
                                                 (Ir_record
                                                   ((0
                                                      (Ir_lit
                                                        (Lit_string what))))))
                                               (Ir_record
                                                 ((0
                                                    (Ir_var
                                                      ((path ())
                                                        (unique_name unit_3)
                                                        (source_name unit))))
                                                   (1
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name tuple2_6)
                                                         (source_name tuple2))))
                                                   (2
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name tuple3_9)
                                                         (source_name tuple3))))
                                                   (3
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name
                                                           tuple4_12)
                                                         (source_name tuple4))))
                                                   (4
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name
                                                           tuple5_15)
                                                         (source_name tuple5))))
                                                   (5
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name
                                                           polyvar0_18)
                                                         (source_name polyvar0))))
                                                   (6
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name
                                                           polyvar1_21)
                                                         (source_name polyvar1))))
                                                   (7
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name
                                                           polyvar2_24)
                                                         (source_name polyvar2))))
                                                   (8
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name r0_36)
                                                         (source_name r0))))
                                                   (9
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name r1_39)
                                                         (source_name r1))))
                                                   (10
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name r2_42)
                                                         (source_name r2))))
                                                   (11
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name f0_45)
                                                         (source_name f0))))
                                                   (12
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name f1_48)
                                                         (source_name f1))))
                                                   (13
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name f2_51)
                                                         (source_name f2))))
                                                   (14
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name f3_54)
                                                         (source_name f3))))
                                                   (15
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name v0_64)
                                                         (source_name v0))))
                                                   (16
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name v1_67)
                                                         (source_name v1))))
                                                   (17
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name v2_70)
                                                         (source_name v2))))
                                                   (18
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name vr_73)
                                                         (source_name vr))))
                                                   (19
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name gadt_78)
                                                         (source_name gadt)))))))))))))))))))))))))))))


================================================================================

NOTE(@ostera): this should be workaround-able, by mapping the syntax to another
construct, like BuckleScript did it with their Js.t type? Dunno, need to check.

We expect objects to not be liftable to IR:

  $ caramel compile --sugarcane --debug lambda_obj.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_obj.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_obj.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_obj.ml.parsetree
  caramel: [DEBUG] OK
  >> Fatal error: Primitive create_object_opt not found.
  [1]

  $ cat lambda_obj.ml.lambda
  cat: lambda_obj.ml.lambda: No such file or directory
  [1]

  $ cat lambda_obj.ml.ir
  cat: lambda_obj.ml.ir: No such file or directory
  [1]

================================================================================

We don't expect assignments to be liftable to IR since they don't have good
support on the Erlang VM.

  $ caramel compile --sugarcane --debug lambda_assign.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_assign.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_assign.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_assign.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_assign.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  TODO: This function has not been implemented yet: Psetfield
  [1]

  $ cat lambda_assign.ml.lambda
  (let
    (makemut/5 = (function param/7 (makemutable 0 (int) 1))
     setfield/8 =
       (function param/10 (setfield_imm 0 (makemutable 0 (int) 1) 2)))
    (makeblock 0 makemut/5 setfield/8))

  $ cat lambda_assign.ml.ir
  cat: lambda_assign.ml.ir: No such file or directory
  [1]

================================================================================

Support for recursive let bindings:

  $ caramel compile --sugarcane --debug lambda_let_rec.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_let_rec.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_let_rec.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_let_rec.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_let_rec.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing lambda_let_rec.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_let_rec.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_let_rec.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat lambda_let_rec.ml.lambda
  (letrec (f/3 (function param/4 (apply f/3 0)))
    (letrec
      (a/5 (function param/7 (apply b/6 0))
        b/6 (function param/8 (apply a/5 0)))
      (let
        (with_aux/9 =
           (function param/13
             (letrec (aux/11 (function param/12 (apply aux/11 0)))
               (apply aux/11 0)))
         g/14 =
           (function param/20
             (letrec
               (a/16 (function param/18 (apply b/17 0))
                 b/17 (function param/19 (apply a/16 0)))
               (apply b/17 0))))
        (makeblock 0 f/3 a/5 b/6 with_aux/9 g/14))))

  $ cat lambda_let_rec.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Lambda_let_rec) (source_name Lambda_let_rec))
       (Ir_letrec
         ((((path ()) (unique_name f_3) (source_name f))
            (Ir_fun (((path ()) (unique_name param_4) (source_name param)))
              (Ir_apply (Ir_var ((path ()) (unique_name f_3) (source_name f)))
                ((Ir_lit (Lit_int 0)))))))
         (Ir_letrec
           ((((path ()) (unique_name a_5) (source_name a))
              (Ir_fun (((path ()) (unique_name param_7) (source_name param)))
                (Ir_apply
                  (Ir_var ((path ()) (unique_name b_6) (source_name b)))
                  ((Ir_lit (Lit_int 0))))))
             (((path ()) (unique_name b_6) (source_name b))
               (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
                 (Ir_apply
                   (Ir_var ((path ()) (unique_name a_5) (source_name a)))
                   ((Ir_lit (Lit_int 0)))))))
           (Ir_let ((path ()) (unique_name with_aux_9) (source_name with_aux))
             (Ir_fun (((path ()) (unique_name param_13) (source_name param)))
               (Ir_letrec
                 ((((path ()) (unique_name aux_11) (source_name aux))
                    (Ir_fun
                      (((path ()) (unique_name param_12) (source_name param)))
                      (Ir_apply
                        (Ir_var
                          ((path ()) (unique_name aux_11) (source_name aux)))
                        ((Ir_lit (Lit_int 0)))))))
                 (Ir_apply
                   (Ir_var ((path ()) (unique_name aux_11) (source_name aux)))
                   ((Ir_lit (Lit_int 0))))))
             (Ir_let ((path ()) (unique_name g_14) (source_name g))
               (Ir_fun (((path ()) (unique_name param_20) (source_name param)))
                 (Ir_letrec
                   ((((path ()) (unique_name a_16) (source_name a))
                      (Ir_fun
                        (((path ()) (unique_name param_18) (source_name param)))
                        (Ir_apply
                          (Ir_var
                            ((path ()) (unique_name b_17) (source_name b)))
                          ((Ir_lit (Lit_int 0))))))
                     (((path ()) (unique_name b_17) (source_name b))
                       (Ir_fun
                         (((path ()) (unique_name param_19)
                            (source_name param)))
                         (Ir_apply
                           (Ir_var
                             ((path ()) (unique_name a_16) (source_name a)))
                           ((Ir_lit (Lit_int 0)))))))
                   (Ir_apply
                     (Ir_var ((path ()) (unique_name b_17) (source_name b)))
                     ((Ir_lit (Lit_int 0))))))
               (Ir_record
                 ((0 (Ir_var ((path ()) (unique_name f_3) (source_name f))))
                   (1 (Ir_var ((path ()) (unique_name a_5) (source_name a))))
                   (2 (Ir_var ((path ()) (unique_name b_6) (source_name b))))
                   (3
                     (Ir_var
                       ((path ()) (unique_name with_aux_9)
                         (source_name with_aux))))
                   (4 (Ir_var ((path ()) (unique_name g_14) (source_name g)))))))))))))

================================================================================

Support for conditionals:

  $ caramel compile --sugarcane --debug lambda_ifthenelse.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_ifthenelse.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_ifthenelse.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_ifthenelse.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_ifthenelse.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing lambda_ifthenelse.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_ifthenelse.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_ifthenelse.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat lambda_ifthenelse.ml.lambda
  (let
    (f/3 = (function param/5 (if 1 [0: 1 1] [0: 2 2]))
     g/6 = (function param/8 (if 0 0 0))
     as_match/9 = (function x/11 (if x/11 0 1))
     catchall0/12 = (function x/14[int] (if (!= x/14 1) 0 1))
     catchall1/15 =
       (function x/17[int] (if (!= x/17 1) (if (!= x/17 2) 0 0) 1))
     catchall2/18 = (function x/20[int] (if (isout 1 (-3+ x/20)) 0 1)))
    (makeblock 0 f/3 g/6 as_match/9 catchall0/12 catchall1/15 catchall2/18))

  $ cat lambda_ifthenelse.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Lambda_ifthenelse)
         (source_name Lambda_ifthenelse))
       (Ir_let ((path ()) (unique_name f_3) (source_name f))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_case (Ir_lit (Lit_int 1))
             (((P_lit (Lit_int 1))
                (Ir_record ((0 (Ir_lit (Lit_int 1))) (1 (Ir_lit (Lit_int 1))))))
               ((P_lit (Lit_int 0))
                 (Ir_record
                   ((0 (Ir_lit (Lit_int 2))) (1 (Ir_lit (Lit_int 2)))))))))
         (Ir_let ((path ()) (unique_name g_6) (source_name g))
           (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
             (Ir_case (Ir_lit (Lit_int 0))
               (((P_lit (Lit_int 1)) (Ir_lit (Lit_int 0)))
                 ((P_lit (Lit_int 0)) (Ir_lit (Lit_int 0))))))
           (Ir_let ((path ()) (unique_name as_match_9) (source_name as_match))
             (Ir_fun (((path ()) (unique_name x_11) (source_name x)))
               (Ir_case (Ir_var ((path ()) (unique_name x_11) (source_name x)))
                 (((P_lit (Lit_int 1)) (Ir_lit (Lit_int 0)))
                   ((P_lit (Lit_int 0)) (Ir_lit (Lit_int 1))))))
             (Ir_let
               ((path ()) (unique_name catchall0_12) (source_name catchall0))
               (Ir_fun (((path ()) (unique_name x_14) (source_name x)))
                 (Ir_case
                   (Ir_ext_call (erlang =/=)
                     ((Ir_var ((path ()) (unique_name x_14) (source_name x)))
                       (Ir_lit (Lit_int 1))))
                   (((P_lit (Lit_int 1)) (Ir_lit (Lit_int 0)))
                     ((P_lit (Lit_int 0)) (Ir_lit (Lit_int 1))))))
               (Ir_let
                 ((path ()) (unique_name catchall1_15) (source_name catchall1))
                 (Ir_fun (((path ()) (unique_name x_17) (source_name x)))
                   (Ir_case
                     (Ir_ext_call (erlang =/=)
                       ((Ir_var ((path ()) (unique_name x_17) (source_name x)))
                         (Ir_lit (Lit_int 1))))
                     (((P_lit (Lit_int 1))
                        (Ir_case
                          (Ir_ext_call (erlang =/=)
                            ((Ir_var
                               ((path ()) (unique_name x_17) (source_name x)))
                              (Ir_lit (Lit_int 2))))
                          (((P_lit (Lit_int 1)) (Ir_lit (Lit_int 0)))
                            ((P_lit (Lit_int 0)) (Ir_lit (Lit_int 0))))))
                       ((P_lit (Lit_int 0)) (Ir_lit (Lit_int 1))))))
                 (Ir_let
                   ((path ()) (unique_name catchall2_18)
                     (source_name catchall2))
                   (Ir_fun (((path ()) (unique_name x_20) (source_name x)))
                     (Ir_case
                       (Ir_ext_call (Caramel.Core.Prim_op is_outside_interval)
                         ((Ir_lit (Lit_int 1))
                           (Ir_ext_call (Caramel.Core.Prim_op offset_int)
                             ((Ir_lit (Lit_int -3))
                               (Ir_var
                                 ((path ()) (unique_name x_20) (source_name x)))))))
                       (((P_lit (Lit_int 1)) (Ir_lit (Lit_int 0)))
                         ((P_lit (Lit_int 0)) (Ir_lit (Lit_int 1))))))
                   (Ir_record
                     ((0
                        (Ir_var ((path ()) (unique_name f_3) (source_name f))))
                       (1
                         (Ir_var ((path ()) (unique_name g_6) (source_name g))))
                       (2
                         (Ir_var
                           ((path ()) (unique_name as_match_9)
                             (source_name as_match))))
                       (3
                         (Ir_var
                           ((path ()) (unique_name catchall0_12)
                             (source_name catchall0))))
                       (4
                         (Ir_var
                           ((path ()) (unique_name catchall1_15)
                             (source_name catchall1))))
                       (5
                         (Ir_var
                           ((path ()) (unique_name catchall2_18)
                             (source_name catchall2)))))))))))))))


================================================================================

Support for external calls

  $ caramel compile --sugarcane --debug lambda_external.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_external.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_external.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_external.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_external.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing lambda_external.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_external.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_external.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat lambda_external.ml.lambda

  $ cat lambda_external.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Lambda_external) (source_name Lambda_external))
       (Ir_let ((path ()) (unique_name g_4) (source_name g))
         (Ir_fun (((path ()) (unique_name param_6) (source_name param)))
           (Ir_ext_call (hello joe) ((Ir_lit (Lit_int 0)))))
         (Ir_record
           ((0 (Ir_var ((path ()) (unique_name g_4) (source_name g))))))))))


================================================================================

Support for calls between modules

  $ caramel compile --sugarcane --debug lambda_modules.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_modules.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_modules.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_modules.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_modules.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing lambda_modules.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_modules.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_modules.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_modules.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_modules.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_modules.ml.b_4
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_modules.A.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_modules.A3.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_modules.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_modules.A2.B.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_modules.A2.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat lambda_modules.ml.lambda
  (let
    (A/6 =
       (module-defn(A/6) Ml lambda_modules.ml(3):25-61
         (let (f/3 = (function _n/5 : int 1)) (makeblock 0 f/3)))
     a/7 = (function param/9 : int (apply (field 0 A/6) 0))
     A2/17 =
       (module-defn(A2/17) Ml lambda_modules.ml(11):107-188
         (let
           (f/10 = (function _n/12 : int 1)
            B/16 =
              (module-defn(B/16) Ml.A2 lambda_modules.ml(14):144-184
                (let (g/13 = (function param/15 : int 2)) (makeblock 0 g/13))))
           (makeblock 0 f/10 B/16)))
     a2/18 = (function param/20 : int (apply (field 0 A2/17) 0))
     A3/28 =
       (module-defn(A3/28) Ml lambda_modules.ml(23):238-319
         (let
           (f/21 = (function _n/23 : int 1)
            include/45 =
              (let (g/24 = (function param/26 : int (apply f/21 0)))
                (makeblock 0 g/24)))
           (makeblock 0 f/21 (field 0 include/45))))
     a3/29 = (function param/31 : int (apply (field 1 A3/28) 0)))
    (makeblock 0 A/6 a/7 A2/17 a2/18 A3/28 a3/29))

  $ cat lambda_modules.ml.ir
  (Ir_program
    ((Ir_module ((path (Lambda_modules)) (unique_name A_6) (source_name A))
       (Ir_let ((path ()) (unique_name f_3) (source_name f))
         (Ir_fun (((path ()) (unique_name _n_5) (source_name _n)))
           (Ir_lit (Lit_int 1)))
         (Ir_record
           ((0 (Ir_var ((path ()) (unique_name f_3) (source_name f))))))))
      (Ir_module ((path (Lambda_modules)) (unique_name A3_28) (source_name A3))
        (Ir_let ((path ()) (unique_name f_21) (source_name f))
          (Ir_fun (((path ()) (unique_name _n_23) (source_name _n)))
            (Ir_lit (Lit_int 1)))
          (Ir_let ((path ()) (unique_name include_45) (source_name include))
            (Ir_let ((path ()) (unique_name g_24) (source_name g))
              (Ir_fun (((path ()) (unique_name param_26) (source_name param)))
                (Ir_apply
                  (Ir_var ((path ()) (unique_name f_21) (source_name f)))
                  ((Ir_lit (Lit_int 0)))))
              (Ir_record
                ((0 (Ir_var ((path ()) (unique_name g_24) (source_name g)))))))
            (Ir_record
              ((0 (Ir_var ((path ()) (unique_name f_21) (source_name f))))
                (1
                  (Ir_field 0
                    (Ir_var
                      ((path ()) (unique_name include_45)
                        (source_name include))))))))))
      (Ir_module
        ((path ()) (unique_name Lambda_modules) (source_name Lambda_modules))
        (Ir_let ((path ()) (unique_name a_7) (source_name a))
          (Ir_fun (((path ()) (unique_name param_9) (source_name param)))
            (Ir_ext_call (Lambda_modules.A f) ((Ir_lit (Lit_int 0)))))
          (Ir_let ((path ()) (unique_name a2_18) (source_name a2))
            (Ir_fun (((path ()) (unique_name param_20) (source_name param)))
              (Ir_ext_call (Lambda_modules.A2 f) ((Ir_lit (Lit_int 0)))))
            (Ir_let ((path ()) (unique_name a3_29) (source_name a3))
              (Ir_fun (((path ()) (unique_name param_31) (source_name param)))
                (Ir_ext_call (Lambda_modules.A3 g) ((Ir_lit (Lit_int 0)))))
              (Ir_record
                ((0 (Ir_var ((path ()) (unique_name A_6) (source_name A))))
                  (1 (Ir_var ((path ()) (unique_name a_7) (source_name a))))
                  (2 (Ir_var ((path ()) (unique_name A2_17) (source_name A2))))
                  (3 (Ir_var ((path ()) (unique_name a2_18) (source_name a2))))
                  (4 (Ir_var ((path ()) (unique_name A3_28) (source_name A3))))
                  (5 (Ir_var ((path ()) (unique_name a3_29) (source_name a3))))))))))
      (Ir_module
        ((path (A2 Lambda_modules)) (unique_name B_16) (source_name B))
        (Ir_let ((path ()) (unique_name g_13) (source_name g))
          (Ir_fun (((path ()) (unique_name param_15) (source_name param)))
            (Ir_lit (Lit_int 2)))
          (Ir_record
            ((0 (Ir_var ((path ()) (unique_name g_13) (source_name g))))))))
      (Ir_module ((path (Lambda_modules)) (unique_name A2_17) (source_name A2))
        (Ir_let ((path ()) (unique_name f_10) (source_name f))
          (Ir_fun (((path ()) (unique_name _n_12) (source_name _n)))
            (Ir_lit (Lit_int 1)))
          (Ir_record
            ((0 (Ir_var ((path ()) (unique_name f_10) (source_name f))))
              (1 (Ir_var ((path ()) (unique_name B_16) (source_name B))))))))))


================================================================================

Support for match/case/switch expressions:

  $ caramel compile --sugarcane --debug lambda_switch.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_switch.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_switch.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_switch.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_switch.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing lambda_switch.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  TODO: This function has not been implemented yet: binary pattern matching missing
  [1]

  $ cat lambda_switch.ml.lambda
  (let
    (switch0/3 =
       (function x/5 : int
         (catch
           (stringswitch x/5
            case "joe": 1
            case "mike": 2
            case "robert": 3
            case "xavier": 4
            default: (exit 1))
          with (1) 5))
     switch1/6 =
       (function x/8[int]
         (if (isout 7 x/8) "other"
           (switch* x/8
            case int 0: "zero"
            case int 1: "one"
            case int 2: "two"
            case int 3: "three"
            case int 4: "four"
            case int 5: "five"
            case int 6: "six"
            case int 7: "seven")))
     switch2/14 =
       (function x/16 : int
         (switch* x/16
          case int 0: 0
          case tag 0: (field 0 x/16)
          case tag 1: (field 1 x/16)
          case tag 2: (field 2 x/16)))
     switch3/20 = (function x/22 : int (if (isint x/22) 0 (field 0 x/22)))
     switch4/30 =
       (function x/32 : int
         (if (!= (field 0 x/32) 1)
           (if (field 1 x/32) 2
             (stringswitch (field 2 x/32) case "hello": 3
                                          default: 0))
           1))
     switch5/35 = (function x/37 (apply (field 1 x/37) (field 0 x/37))))
    (makeblock 0 switch0/3 switch1/6 switch2/14 switch3/20 switch4/30
      switch5/35))

  $ cat lambda_switch.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Lambda_switch) (source_name Lambda_switch))
       (Ir_let ((path ()) (unique_name switch0_3) (source_name switch0))
         (Ir_fun (((path ()) (unique_name x_5) (source_name x)))
           (Ir_catch
             (Ir_case (Ir_var ((path ()) (unique_name x_5) (source_name x)))
               (((P_lit (Lit_string joe)) (Ir_lit (Lit_int 1)))
                 ((P_lit (Lit_string mike)) (Ir_lit (Lit_int 2)))
                 ((P_lit (Lit_string robert)) (Ir_lit (Lit_int 3)))
                 ((P_lit (Lit_string xavier)) (Ir_lit (Lit_int 4)))
                 (P_ignore (Ir_throw 1 ()))))
             (Ir_lit (Lit_int 5))))
         (Ir_let ((path ()) (unique_name switch1_6) (source_name switch1))
           (Ir_fun (((path ()) (unique_name x_8) (source_name x)))
             (Ir_case
               (Ir_ext_call (Caramel.Core.Prim_op is_outside_interval)
                 ((Ir_lit (Lit_int 7))
                   (Ir_var ((path ()) (unique_name x_8) (source_name x)))))
               (((P_lit (Lit_int 1)) (Ir_lit (Lit_string other)))
                 ((P_lit (Lit_int 0))
                   (Ir_case
                     (Ir_var ((path ()) (unique_name x_8) (source_name x)))
                     (((P_lit (Lit_int 0)) (Ir_lit (Lit_string zero)))
                       ((P_lit (Lit_int 1)) (Ir_lit (Lit_string one)))
                       ((P_lit (Lit_int 2)) (Ir_lit (Lit_string two)))
                       ((P_lit (Lit_int 3)) (Ir_lit (Lit_string three)))
                       ((P_lit (Lit_int 4)) (Ir_lit (Lit_string four)))
                       ((P_lit (Lit_int 5)) (Ir_lit (Lit_string five)))
                       ((P_lit (Lit_int 6)) (Ir_lit (Lit_string six)))
                       ((P_lit (Lit_int 7)) (Ir_lit (Lit_string seven)))))))))
           (Ir_let ((path ()) (unique_name switch2_14) (source_name switch2))
             (Ir_fun (((path ()) (unique_name x_16) (source_name x)))
               (Ir_case (Ir_var ((path ()) (unique_name x_16) (source_name x)))
                 (((P_lit (Lit_int 0)) (Ir_lit (Lit_int 0)))
                   ((P_lit (Lit_int 0))
                     (Ir_field 0
                       (Ir_var ((path ()) (unique_name x_16) (source_name x)))))
                   ((P_lit (Lit_int 1))
                     (Ir_field 1
                       (Ir_var ((path ()) (unique_name x_16) (source_name x)))))
                   ((P_lit (Lit_int 2))
                     (Ir_field 2
                       (Ir_var ((path ()) (unique_name x_16) (source_name x))))))))
             (Ir_let ((path ()) (unique_name switch3_20) (source_name switch3))
               (Ir_fun (((path ()) (unique_name x_22) (source_name x)))
                 (Ir_case
                   (Ir_ext_call (erlang is_integer)
                     ((Ir_var ((path ()) (unique_name x_22) (source_name x)))))
                   (((P_lit (Lit_int 1)) (Ir_lit (Lit_int 0)))
                     ((P_lit (Lit_int 0))
                       (Ir_field 0
                         (Ir_var
                           ((path ()) (unique_name x_22) (source_name x))))))))
               (Ir_let
                 ((path ()) (unique_name switch4_30) (source_name switch4))
                 (Ir_fun (((path ()) (unique_name x_32) (source_name x)))
                   (Ir_case
                     (Ir_ext_call (erlang =/=)
                       ((Ir_field 0
                          (Ir_var
                            ((path ()) (unique_name x_32) (source_name x))))
                         (Ir_lit (Lit_int 1))))
                     (((P_lit (Lit_int 1))
                        (Ir_case
                          (Ir_field 1
                            (Ir_var
                              ((path ()) (unique_name x_32) (source_name x))))
                          (((P_lit (Lit_int 1)) (Ir_lit (Lit_int 2)))
                            ((P_lit (Lit_int 0))
                              (Ir_case
                                (Ir_field 2
                                  (Ir_var
                                    ((path ()) (unique_name x_32)
                                      (source_name x))))
                                (((P_lit (Lit_string hello))
                                   (Ir_lit (Lit_int 3)))
                                  (P_ignore (Ir_lit (Lit_int 0)))))))))
                       ((P_lit (Lit_int 0)) (Ir_lit (Lit_int 1))))))
                 (Ir_let
                   ((path ()) (unique_name switch5_35) (source_name switch5))
                   (Ir_fun (((path ()) (unique_name x_37) (source_name x)))
                     (Ir_apply
                       (Ir_field 1
                         (Ir_var
                           ((path ()) (unique_name x_37) (source_name x))))
                       ((Ir_field 0
                          (Ir_var
                            ((path ()) (unique_name x_37) (source_name x)))))))
                   (Ir_record
                     ((0
                        (Ir_var
                          ((path ()) (unique_name switch0_3)
                            (source_name switch0))))
                       (1
                         (Ir_var
                           ((path ()) (unique_name switch1_6)
                             (source_name switch1))))
                       (2
                         (Ir_var
                           ((path ()) (unique_name switch2_14)
                             (source_name switch2))))
                       (3
                         (Ir_var
                           ((path ()) (unique_name switch3_20)
                             (source_name switch3))))
                       (4
                         (Ir_var
                           ((path ()) (unique_name switch4_30)
                             (source_name switch4))))
                       (5
                         (Ir_var
                           ((path ()) (unique_name switch5_35)
                             (source_name switch5)))))))))))))))


================================================================================

Support for sequences of expressions;

  $ caramel compile --sugarcane --debug lambda_sequence.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_sequence.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_sequence.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_sequence.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_sequence.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing lambda_sequence.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_sequence.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Lambda_sequence.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat lambda_sequence.ml.lambda
  (letrec
    (f/3 (function param/4 (seq (apply f/3 0) (apply f/3 0) (apply f/3 0) 0)))
    (makeblock 0 f/3))

  $ cat lambda_sequence.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Lambda_sequence) (source_name Lambda_sequence))
       (Ir_letrec
         ((((path ()) (unique_name f_3) (source_name f))
            (Ir_fun (((path ()) (unique_name param_4) (source_name param)))
              (Ir_seq
                (Ir_apply
                  (Ir_var ((path ()) (unique_name f_3) (source_name f)))
                  ((Ir_lit (Lit_int 0))))
                (Ir_seq
                  (Ir_apply
                    (Ir_var ((path ()) (unique_name f_3) (source_name f)))
                    ((Ir_lit (Lit_int 0))))
                  (Ir_seq
                    (Ir_apply
                      (Ir_var ((path ()) (unique_name f_3) (source_name f)))
                      ((Ir_lit (Lit_int 0))))
                    (Ir_lit (Lit_int 0))))))))
         (Ir_record
           ((0 (Ir_var ((path ()) (unique_name f_3) (source_name f))))))))))


================================================================================

Sample. To add a new test, copy and paste this above, and replace `_.ml` with
the right .ml file. Then remove the `#` that are preventing these commands from
running.

  $ # caramel compile --sugarcane --debug _.ml

  $ # cat _.ml.lambda

  $ # cat _.ml.ir


