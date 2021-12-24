================================================================================

We expect to not be able to compile top-level module values that are not
functions:

  $ caramel compile --debug lambda_let_val.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_let_val.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_let_val.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_let_val.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_let_val.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing lambda_let_val.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  PANIC: we expected a let binding here!
  caramel: internal error, uncaught exception:
                                                  "Assert_failure caramel/newcomp/sugarcane/error.ml:22:2"
                                                  
  [125]

  $ cat lambda_let_val.ml.lambda

  $ cat lambda_let_val.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Lambda_let_val)
         (source_name Caramel.Lambda_let_val))
       (Ir_let Exported ((path ()) (unique_name a_3) (source_name a))
         (Ir_lit (Lit_int 1))
         (Ir_tuple ((Ir_var ((path ()) (unique_name a_3) (source_name a)))))))))

================================================================================

Top-level module functions, however, should be lifted to the IR:

  $ caramel compile --debug lambda_let_fun.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_let_fun.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_let_fun.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_let_fun.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_let_fun.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing lambda_let_fun.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_let_fun.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Lambda_let_fun.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat lambda_let_fun.ml.lambda

  $ cat lambda_let_fun.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Lambda_let_fun)
         (source_name Caramel.Lambda_let_fun))
       (Ir_let Exported ((path ()) (unique_name a_3) (source_name a))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_int 1)))
         (Ir_tuple ((Ir_var ((path ()) (unique_name a_3) (source_name a)))))))))


================================================================================

Support for function application:

  $ caramel compile --debug lambda_apply.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_apply.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_apply.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_apply.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_apply.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing lambda_apply.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_apply.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Lambda_apply.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat lambda_apply.ml.lambda
  (let
    (a/3 = (function param/5 : int 1)
     x/6 = (function param/8 : int (apply a/3 0)))

  $ cat lambda_apply.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Lambda_apply)
         (source_name Caramel.Lambda_apply))
       (Ir_let Exported ((path ()) (unique_name a_3) (source_name a))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_int 1)))
         (Ir_let Exported ((path ()) (unique_name x_6) (source_name x))
           (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
             (Ir_apply
               (Ir_fn_name ((path ()) (unique_name a_3) (source_name a)) 1)
               ((Ir_lit (Lit_atom unit)))))
           (Ir_tuple
             ((Ir_var ((path ()) (unique_name a_3) (source_name a)))
               (Ir_var ((path ()) (unique_name x_6) (source_name x))))))))))


================================================================================

Support for literals:

  $ caramel compile --debug lambda_const.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_const.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_const.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_const.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_const.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing lambda_const.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_const.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Lambda_const.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

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
       ((path ()) (unique_name Caramel.Lambda_const)
         (source_name Caramel.Lambda_const))
       (Ir_let Exported ((path ()) (unique_name int_3) (source_name int))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_int 0)))
         (Ir_let Exported
           ((path ()) (unique_name nativeint_6) (source_name nativeint))
           (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
             (Ir_lit (Lit_int 0)))
           (Ir_let Exported
             ((path ()) (unique_name int32_9) (source_name int32))
             (Ir_fun (((path ()) (unique_name param_11) (source_name param)))
               (Ir_lit (Lit_int 0)))
             (Ir_let Exported
               ((path ()) (unique_name int64_12) (source_name int64))
               (Ir_fun (((path ()) (unique_name param_14) (source_name param)))
                 (Ir_lit (Lit_int 0)))
               (Ir_let Exported
                 ((path ()) (unique_name float_15) (source_name float))
                 (Ir_fun
                   (((path ()) (unique_name param_17) (source_name param)))
                   (Ir_lit (Lit_float 0.0123412)))
                 (Ir_let Exported
                   ((path ()) (unique_name char_18) (source_name char))
                   (Ir_fun
                     (((path ()) (unique_name param_20) (source_name param)))
                     (Ir_lit (Lit_char a)))
                   (Ir_let Exported
                     ((path ()) (unique_name string_21) (source_name string))
                     (Ir_fun
                       (((path ()) (unique_name param_23) (source_name param)))
                       (Ir_lit (Lit_string "hello world")))
                     (Ir_let Exported
                       ((path ()) (unique_name multiline_string_24)
                         (source_name multiline_string))
                       (Ir_fun
                         (((path ()) (unique_name param_26)
                            (source_name param)))
                         (Ir_lit (Lit_string  "\
                                             \nhello world\
                                             \n\
                                             \n")))
                       (Ir_let Exported
                         ((path ()) (unique_name bool_true_27)
                           (source_name bool_true))
                         (Ir_fun
                           (((path ()) (unique_name param_29)
                              (source_name param)))
                           (Ir_lit (Lit_atom true)))
                         (Ir_let Exported
                           ((path ()) (unique_name bool_false_30)
                             (source_name bool_false))
                           (Ir_fun
                             (((path ()) (unique_name param_32)
                                (source_name param)))
                             (Ir_lit (Lit_atom false)))
                           (Ir_tuple
                             ((Ir_var
                                ((path ()) (unique_name int_3)
                                  (source_name int)))
                               (Ir_var
                                 ((path ()) (unique_name nativeint_6)
                                   (source_name nativeint)))
                               (Ir_var
                                 ((path ()) (unique_name int32_9)
                                   (source_name int32)))
                               (Ir_var
                                 ((path ()) (unique_name int64_12)
                                   (source_name int64)))
                               (Ir_var
                                 ((path ()) (unique_name float_15)
                                   (source_name float)))
                               (Ir_var
                                 ((path ()) (unique_name char_18)
                                   (source_name char)))
                               (Ir_var
                                 ((path ()) (unique_name string_21)
                                   (source_name string)))
                               (Ir_var
                                 ((path ()) (unique_name multiline_string_24)
                                   (source_name multiline_string)))
                               (Ir_var
                                 ((path ()) (unique_name bool_true_27)
                                   (source_name bool_true)))
                               (Ir_var
                                 ((path ()) (unique_name bool_false_30)
                                   (source_name bool_false))))))))))))))))))


================================================================================

Functions and lambdas!

  $ caramel compile --debug lambda_fun.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_fun.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_fun.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_fun.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_fun.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing lambda_fun.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_fun.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Lambda_fun.core
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
    ((Ir_module
       ((path ()) (unique_name Caramel.Lambda_fun)
         (source_name Caramel.Lambda_fun))
       (Ir_let Exported ((path ()) (unique_name f_3) (source_name f))
         (Ir_fun
           (((path ()) (unique_name _arg0_5) (source_name _arg0))
             ((path ()) (unique_name _arg1_6) (source_name _arg1)))
           (Ir_let Private
             ((path ()) (unique_name lambda_7) (source_name lambda))
             (Ir_fun
               (((path ()) (unique_name x1_9) (source_name x1))
                 ((path ()) (unique_name _x2_10) (source_name _x2))
                 ((path ()) (unique_name _x3_11) (source_name _x3)))
               (Ir_var ((path ()) (unique_name x1_9) (source_name x1))))
             (Ir_apply
               (Ir_var ((path ()) (unique_name lambda_7) (source_name lambda)))
               ((Ir_lit (Lit_atom unit))))))
         (Ir_let Exported ((path ()) (unique_name g_12) (source_name g))
           (Ir_fun (((path ()) (unique_name _arg0_14) (source_name _arg0)))
             (Ir_apply
               (Ir_fn_name ((path ()) (unique_name f_3) (source_name f)) 2)
               ((Ir_lit (Lit_atom unit)))))
           (Ir_tuple
             ((Ir_var ((path ()) (unique_name f_3) (source_name f)))
               (Ir_var ((path ()) (unique_name g_12) (source_name g))))))))))

================================================================================

NOTE(@ostera): this should be workaround-able, by mapping the syntax to another
construct, like BuckleScript did it with their Js.t type? Dunno, need to check.

We expect objects to not be liftable to IR:

  $ caramel compile --debug lambda_obj.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_obj.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_obj.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_obj.ml.parsetree
  caramel: [DEBUG] OK
  >> Fatal error: Primitive create_object_opt not found.
  ERROR: Misc.Fatal_error
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

  $ caramel compile --debug lambda_assign.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_assign.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_assign.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_assign.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_assign.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
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

  $ caramel compile --debug lambda_let_rec.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_let_rec.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_let_rec.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_let_rec.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_let_rec.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing lambda_let_rec.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  TODO: This function has not been implemented yet: Ir_letrec
  [1]

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
       ((path ()) (unique_name Caramel.Lambda_let_rec)
         (source_name Caramel.Lambda_let_rec))
       (Ir_letrec
         ((Exported ((path ()) (unique_name f_3) (source_name f))
            (Ir_fun (((path ()) (unique_name param_4) (source_name param)))
              (Ir_apply
                (Ir_fn_name ((path ()) (unique_name f_3) (source_name f)) 1)
                ((Ir_lit (Lit_atom unit)))))))
         (Ir_letrec
           ((Exported ((path ()) (unique_name a_5) (source_name a))
              (Ir_fun (((path ()) (unique_name param_7) (source_name param)))
                (Ir_apply
                  (Ir_fn_name ((path ()) (unique_name b_6) (source_name b)) 1)
                  ((Ir_lit (Lit_atom unit))))))
             (Exported ((path ()) (unique_name b_6) (source_name b))
               (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
                 (Ir_apply
                   (Ir_fn_name ((path ()) (unique_name a_5) (source_name a)) 1)
                   ((Ir_lit (Lit_atom unit)))))))
           (Ir_let Exported
             ((path ()) (unique_name with_aux_9) (source_name with_aux))
             (Ir_fun (((path ()) (unique_name param_13) (source_name param)))
               (Ir_letrec
                 ((Private ((path ()) (unique_name aux_11) (source_name aux))
                    (Ir_fun
                      (((path ()) (unique_name param_12) (source_name param)))
                      (Ir_apply
                        (Ir_var
                          ((path ()) (unique_name aux_11) (source_name aux)))
                        ((Ir_lit (Lit_atom unit)))))))
                 (Ir_apply
                   (Ir_var ((path ()) (unique_name aux_11) (source_name aux)))
                   ((Ir_lit (Lit_atom unit))))))
             (Ir_let Exported ((path ()) (unique_name g_14) (source_name g))
               (Ir_fun (((path ()) (unique_name param_20) (source_name param)))
                 (Ir_letrec
                   ((Exported ((path ()) (unique_name a_16) (source_name a))
                      (Ir_fun
                        (((path ()) (unique_name param_18) (source_name param)))
                        (Ir_apply
                          (Ir_var
                            ((path ()) (unique_name b_17) (source_name b)))
                          ((Ir_lit (Lit_atom unit))))))
                     (Exported ((path ()) (unique_name b_17) (source_name b))
                       (Ir_fun
                         (((path ()) (unique_name param_19)
                            (source_name param)))
                         (Ir_apply
                           (Ir_var
                             ((path ()) (unique_name a_16) (source_name a)))
                           ((Ir_lit (Lit_atom unit)))))))
                   (Ir_apply
                     (Ir_var ((path ()) (unique_name b_17) (source_name b)))
                     ((Ir_lit (Lit_atom unit))))))
               (Ir_tuple
                 ((Ir_var ((path ()) (unique_name f_3) (source_name f)))
                   (Ir_var ((path ()) (unique_name a_5) (source_name a)))
                   (Ir_var ((path ()) (unique_name b_6) (source_name b)))
                   (Ir_var
                     ((path ()) (unique_name with_aux_9)
                       (source_name with_aux)))
                   (Ir_var ((path ()) (unique_name g_14) (source_name g))))))))))))

================================================================================

Support for conditionals:

  $ caramel compile --debug lambda_ifthenelse.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_ifthenelse.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_ifthenelse.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_ifthenelse.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_ifthenelse.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing lambda_ifthenelse.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_ifthenelse.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Lambda_ifthenelse.core
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
       ((path ()) (unique_name Caramel.Lambda_ifthenelse)
         (source_name Caramel.Lambda_ifthenelse))
       (Ir_let Exported ((path ()) (unique_name f_3) (source_name f))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_case (Ir_lit (Lit_atom true))
             (((P_lit (Lit_atom true))
                (Ir_tuple ((Ir_lit (Lit_int 1)) (Ir_lit (Lit_int 1)))))
               (P_ignore
                 (Ir_tuple ((Ir_lit (Lit_int 2)) (Ir_lit (Lit_int 2))))))))
         (Ir_let Exported ((path ()) (unique_name g_6) (source_name g))
           (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
             (Ir_case (Ir_lit (Lit_atom false))
               (((P_lit (Lit_atom true)) (Ir_lit (Lit_atom unit)))
                 (P_ignore (Ir_lit (Lit_int 0))))))
           (Ir_let Exported
             ((path ()) (unique_name as_match_9) (source_name as_match))
             (Ir_fun (((path ()) (unique_name x_11) (source_name x)))
               (Ir_case (Ir_var ((path ()) (unique_name x_11) (source_name x)))
                 (((P_lit (Lit_atom true)) (Ir_lit (Lit_atom false)))
                   (P_ignore (Ir_lit (Lit_atom true))))))
             (Ir_let Exported
               ((path ()) (unique_name catchall0_12) (source_name catchall0))
               (Ir_fun (((path ()) (unique_name x_14) (source_name x)))
                 (Ir_case
                   (Ir_ext_call (erlang =/=)
                     ((Ir_var ((path ()) (unique_name x_14) (source_name x)))
                       (Ir_lit (Lit_int 1))))
                   (((P_lit (Lit_atom true)) (Ir_lit (Lit_atom false)))
                     (P_ignore (Ir_lit (Lit_atom true))))))
               (Ir_let Exported
                 ((path ()) (unique_name catchall1_15) (source_name catchall1))
                 (Ir_fun (((path ()) (unique_name x_17) (source_name x)))
                   (Ir_case
                     (Ir_ext_call (erlang =/=)
                       ((Ir_var ((path ()) (unique_name x_17) (source_name x)))
                         (Ir_lit (Lit_int 1))))
                     (((P_lit (Lit_atom true))
                        (Ir_case
                          (Ir_ext_call (erlang =/=)
                            ((Ir_var
                               ((path ()) (unique_name x_17) (source_name x)))
                              (Ir_lit (Lit_int 2))))
                          (((P_lit (Lit_atom true)) (Ir_lit (Lit_atom false)))
                            (P_ignore (Ir_lit (Lit_atom false))))))
                       (P_ignore (Ir_lit (Lit_atom true))))))
                 (Ir_let Exported
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
                       (((P_lit (Lit_atom true)) (Ir_lit (Lit_atom false)))
                         (P_ignore (Ir_lit (Lit_atom true))))))
                   (Ir_tuple
                     ((Ir_var ((path ()) (unique_name f_3) (source_name f)))
                       (Ir_var ((path ()) (unique_name g_6) (source_name g)))
                       (Ir_var
                         ((path ()) (unique_name as_match_9)
                           (source_name as_match)))
                       (Ir_var
                         ((path ()) (unique_name catchall0_12)
                           (source_name catchall0)))
                       (Ir_var
                         ((path ()) (unique_name catchall1_15)
                           (source_name catchall1)))
                       (Ir_var
                         ((path ()) (unique_name catchall2_18)
                           (source_name catchall2))))))))))))))


================================================================================

Support for external calls

  $ caramel compile --debug lambda_external.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_external.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_external.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_external.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_external.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing lambda_external.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_external.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Lambda_external.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat lambda_external.ml.lambda

  $ cat lambda_external.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Lambda_external)
         (source_name Caramel.Lambda_external))
       (Ir_let Exported ((path ()) (unique_name g_4) (source_name g))
         (Ir_fun (((path ()) (unique_name param_6) (source_name param)))
           (Ir_ext_call (hello joe) ((Ir_lit (Lit_atom unit)))))
         (Ir_tuple ((Ir_var ((path ()) (unique_name g_4) (source_name g)))))))))


================================================================================

Support for calls between modules

  $ caramel compile --debug lambda_modules.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_modules.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_modules.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_modules.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_modules.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing lambda_modules.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  PANIC: we expected a let binding for a function here!
  caramel: internal error, uncaught exception:
                                                                 "Assert_failure caramel/newcomp/sugarcane/error.ml:22:2"
                                                                 
  [125]

  $ cat lambda_modules.ml.lambda
  (let
    (A/6 =
       (module-defn(A/6) Lambda_modules lambda_modules.ml(3):25-61
         (let (f/3 = (function _n/5 : int 1)) (makeblock 0 f/3)))
     a/7 = (function param/9 : int (apply (field 0 A/6) 0))
     A2/17 =
       (module-defn(A2/17) Lambda_modules lambda_modules.ml(11):107-188
         (let
           (f/10 = (function _n/12 : int 1)
            B/16 =
              (module-defn(B/16) Lambda_modules.A2 lambda_modules.ml(14):144-184
                (let (g/13 = (function param/15 : int 2)) (makeblock 0 g/13))))
           (makeblock 0 f/10 B/16)))
     a2/18 = (function param/20 : int (apply (field 0 A2/17) 0))
     A3/28 =
       (module-defn(A3/28) Lambda_modules lambda_modules.ml(23):238-319
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
    ((Ir_module
       ((path (Caramel.Lambda_modules)) (unique_name A_6) (source_name A))
       (Ir_let Exported ((path ()) (unique_name f_3) (source_name f))
         (Ir_fun (((path ()) (unique_name _n_5) (source_name _n)))
           (Ir_lit (Lit_int 1)))
         (Ir_tuple ((Ir_var ((path ()) (unique_name f_3) (source_name f)))))))
      (Ir_module
        ((path (Caramel.Lambda_modules)) (unique_name A3_28) (source_name A3))
        (Ir_let Exported ((path ()) (unique_name f_21) (source_name f))
          (Ir_fun (((path ()) (unique_name _n_23) (source_name _n)))
            (Ir_lit (Lit_int 1)))
          (Ir_let Private
            ((path ()) (unique_name include_45) (source_name include))
            (Ir_let Exported ((path ()) (unique_name g_24) (source_name g))
              (Ir_fun (((path ()) (unique_name param_26) (source_name param)))
                (Ir_apply
                  (Ir_fn_name ((path ()) (unique_name f_21) (source_name f)) 1)
                  ((Ir_lit (Lit_atom unit)))))
              (Ir_tuple
                ((Ir_var ((path ()) (unique_name g_24) (source_name g))))))
            (Ir_tuple
              ((Ir_var ((path ()) (unique_name f_21) (source_name f)))
                (Ir_ext_call (erlang element)
                  ((Ir_lit (Lit_int 1))
                    (Ir_var
                      ((path ()) (unique_name include_45)
                        (source_name include))))))))))
      (Ir_module
        ((path (Caramel.Lambda_modules)) (unique_name A2_17) (source_name A2))
        (Ir_let Exported ((path ()) (unique_name f_10) (source_name f))
          (Ir_fun (((path ()) (unique_name _n_12) (source_name _n)))
            (Ir_lit (Lit_int 1)))
          (Ir_tuple
            ((Ir_var ((path ()) (unique_name f_10) (source_name f)))
              (Ir_var ((path ()) (unique_name B_16) (source_name B)))))))
      (Ir_module
        ((path ()) (unique_name Caramel.Lambda_modules)
          (source_name Caramel.Lambda_modules))
        (Ir_let Exported ((path ()) (unique_name a_7) (source_name a))
          (Ir_fun (((path ()) (unique_name param_9) (source_name param)))
            (Ir_ext_call (Caramel.Lambda_modules.A f)
              ((Ir_lit (Lit_atom unit)))))
          (Ir_let Exported ((path ()) (unique_name a2_18) (source_name a2))
            (Ir_fun (((path ()) (unique_name param_20) (source_name param)))
              (Ir_ext_call (Caramel.Lambda_modules.A2 f)
                ((Ir_lit (Lit_atom unit)))))
            (Ir_let Exported ((path ()) (unique_name a3_29) (source_name a3))
              (Ir_fun (((path ()) (unique_name param_31) (source_name param)))
                (Ir_ext_call (Caramel.Lambda_modules.A3 g)
                  ((Ir_lit (Lit_atom unit)))))
              (Ir_tuple
                ((Ir_var ((path ()) (unique_name A_6) (source_name A)))
                  (Ir_var ((path ()) (unique_name a_7) (source_name a)))
                  (Ir_var ((path ()) (unique_name A2_17) (source_name A2)))
                  (Ir_var ((path ()) (unique_name a2_18) (source_name a2)))
                  (Ir_var ((path ()) (unique_name A3_28) (source_name A3)))
                  (Ir_var ((path ()) (unique_name a3_29) (source_name a3)))))))))
      (Ir_module
        ((path (A2 Caramel.Lambda_modules)) (unique_name B_16) (source_name B))
        (Ir_let Exported ((path ()) (unique_name g_13) (source_name g))
          (Ir_fun (((path ()) (unique_name param_15) (source_name param)))
            (Ir_lit (Lit_int 2)))
          (Ir_tuple ((Ir_var ((path ()) (unique_name g_13) (source_name g)))))))))


================================================================================

Support for match/case/switch expressions:

  $ caramel compile --debug lambda_switch.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_switch.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_switch.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_switch.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_switch.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] constructor field access Hello/2
  caramel: [DEBUG] constructor field access Hello/2
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 2
  caramel: [DEBUG] constructor field access V1/1
  caramel: [DEBUG] constructor field access V2/2
  caramel: [DEBUG] constructor field access V3/3
  caramel: [DEBUG] constructor field access V1/1
  caramel: [DEBUG] constructor field access V2/2
  caramel: [DEBUG] constructor field access V3/3
  caramel: [DEBUG] Writing lambda_switch.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_switch.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Lambda_switch.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

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
     switch3/20 =
       (function x/22 : int
         (switch* x/22
          case int 0: 0
          case tag 0: (field 0 x/22)
          case tag 1: (field 0 x/22)
          case tag 2: (field 0 x/22)))
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
       ((path ()) (unique_name Caramel.Lambda_switch)
         (source_name Caramel.Lambda_switch))
       (Ir_let Exported
         ((path ()) (unique_name switch0_3) (source_name switch0))
         (Ir_fun (((path ()) (unique_name x_5) (source_name x)))
           (Ir_catch
             (Ir_case (Ir_var ((path ()) (unique_name x_5) (source_name x)))
               (((P_lit (Lit_string joe)) (Ir_lit (Lit_int 1)))
                 ((P_lit (Lit_string mike)) (Ir_lit (Lit_int 2)))
                 ((P_lit (Lit_string robert)) (Ir_lit (Lit_int 3)))
                 ((P_lit (Lit_string xavier)) (Ir_lit (Lit_int 4)))
                 (P_ignore (Ir_throw 1 ()))))
             (Ir_lit (Lit_int 5))))
         (Ir_let Exported
           ((path ()) (unique_name switch1_6) (source_name switch1))
           (Ir_fun (((path ()) (unique_name x_8) (source_name x)))
             (Ir_case
               (Ir_ext_call (Caramel.Core.Prim_op is_outside_interval)
                 ((Ir_lit (Lit_int 7))
                   (Ir_var ((path ()) (unique_name x_8) (source_name x)))))
               (((P_lit (Lit_atom true)) (Ir_lit (Lit_string other)))
                 (P_ignore
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
           (Ir_let Exported
             ((path ()) (unique_name switch2_14) (source_name switch2))
             (Ir_fun (((path ()) (unique_name x_16) (source_name x)))
               (Ir_case (Ir_var ((path ()) (unique_name x_16) (source_name x)))
                 (((P_tuple ((P_lit (Lit_atom v)))) (Ir_lit (Lit_int 0)))
                   ((P_tuple ((P_lit (Lit_atom v))))
                     (Ir_ext_call (erlang element)
                       ((Ir_lit (Lit_int 2))
                         (Ir_var
                           ((path ()) (unique_name x_16) (source_name x))))))
                   ((P_tuple ((P_lit (Lit_atom v1)) P_ignore))
                     (Ir_ext_call (erlang element)
                       ((Ir_lit (Lit_int 3))
                         (Ir_var
                           ((path ()) (unique_name x_16) (source_name x))))))
                   ((P_tuple ((P_lit (Lit_atom v2)) P_ignore (P_bind _2)))
                     (Ir_ext_call (erlang element)
                       ((Ir_lit (Lit_int 4))
                         (Ir_var
                           ((path ()) (unique_name x_16) (source_name x)))))))))
             (Ir_let Exported
               ((path ()) (unique_name switch3_20) (source_name switch3))
               (Ir_fun (((path ()) (unique_name x_22) (source_name x)))
                 (Ir_case
                   (Ir_var ((path ()) (unique_name x_22) (source_name x)))
                   (((P_tuple ((P_lit (Lit_atom v)))) (Ir_lit (Lit_int 0)))
                     ((P_tuple ((P_lit (Lit_atom v))))
                       (Ir_ext_call (erlang element)
                         ((Ir_lit (Lit_int 2))
                           (Ir_var
                             ((path ()) (unique_name x_22) (source_name x))))))
                     ((P_tuple ((P_lit (Lit_atom v1)) P_ignore))
                       (Ir_ext_call (erlang element)
                         ((Ir_lit (Lit_int 2))
                           (Ir_var
                             ((path ()) (unique_name x_22) (source_name x))))))
                     ((P_tuple ((P_lit (Lit_atom v2)) P_ignore (P_bind _2)))
                       (Ir_ext_call (erlang element)
                         ((Ir_lit (Lit_int 2))
                           (Ir_var
                             ((path ()) (unique_name x_22) (source_name x)))))))))
               (Ir_let Exported
                 ((path ()) (unique_name switch4_30) (source_name switch4))
                 (Ir_fun (((path ()) (unique_name x_32) (source_name x)))
                   (Ir_case
                     (Ir_ext_call (erlang =/=)
                       ((Ir_ext_call (erlang element)
                          ((Ir_lit (Lit_int 1))
                            (Ir_var
                              ((path ()) (unique_name x_32) (source_name x)))))
                         (Ir_lit (Lit_int 1))))
                     (((P_lit (Lit_atom true))
                        (Ir_case
                          (Ir_ext_call (erlang element)
                            ((Ir_lit (Lit_int 2))
                              (Ir_var
                                ((path ()) (unique_name x_32) (source_name x)))))
                          (((P_lit (Lit_atom true)) (Ir_lit (Lit_int 2)))
                            (P_ignore
                              (Ir_case
                                (Ir_ext_call (erlang element)
                                  ((Ir_lit (Lit_int 3))
                                    (Ir_var
                                      ((path ()) (unique_name x_32)
                                        (source_name x)))))
                                (((P_lit (Lit_string hello))
                                   (Ir_lit (Lit_int 3)))
                                  (P_ignore (Ir_lit (Lit_int 0)))))))))
                       (P_ignore (Ir_lit (Lit_int 1))))))
                 (Ir_let Exported
                   ((path ()) (unique_name switch5_35) (source_name switch5))
                   (Ir_fun (((path ()) (unique_name x_37) (source_name x)))
                     (Ir_apply
                       (Ir_ext_call (erlang element)
                         ((Ir_lit (Lit_int 3))
                           (Ir_var
                             ((path ()) (unique_name x_37) (source_name x)))))
                       ((Ir_ext_call (erlang element)
                          ((Ir_lit (Lit_int 2))
                            (Ir_var
                              ((path ()) (unique_name x_37) (source_name x))))))))
                   (Ir_tuple
                     ((Ir_var
                        ((path ()) (unique_name switch0_3)
                          (source_name switch0)))
                       (Ir_var
                         ((path ()) (unique_name switch1_6)
                           (source_name switch1)))
                       (Ir_var
                         ((path ()) (unique_name switch2_14)
                           (source_name switch2)))
                       (Ir_var
                         ((path ()) (unique_name switch3_20)
                           (source_name switch3)))
                       (Ir_var
                         ((path ()) (unique_name switch4_30)
                           (source_name switch4)))
                       (Ir_var
                         ((path ()) (unique_name switch5_35)
                           (source_name switch5))))))))))))))


================================================================================

Support for sequences of expressions;

  $ caramel compile --debug lambda_sequence.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (lambda_sequence.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file lambda_sequence.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing lambda_sequence.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing lambda_sequence.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing lambda_sequence.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing lambda_sequence.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Lambda_sequence.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat lambda_sequence.ml.lambda
  (letrec
    (f/3 (function param/4 (seq (apply f/3 0) (apply f/3 0) (apply f/3 0) 0)))
    (makeblock 0 f/3))

  $ cat lambda_sequence.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Lambda_sequence)
         (source_name Caramel.Lambda_sequence))
       (Ir_letrec
         ((Exported ((path ()) (unique_name f_3) (source_name f))
            (Ir_fun (((path ()) (unique_name param_4) (source_name param)))
              (Ir_seq
                (Ir_apply
                  (Ir_fn_name ((path ()) (unique_name f_3) (source_name f)) 1)
                  ((Ir_lit (Lit_atom unit))))
                (Ir_seq
                  (Ir_apply
                    (Ir_fn_name ((path ()) (unique_name f_3) (source_name f))
                      1)
                    ((Ir_lit (Lit_atom unit))))
                  (Ir_seq
                    (Ir_apply
                      (Ir_fn_name ((path ()) (unique_name f_3) (source_name f))
                        1)
                      ((Ir_lit (Lit_atom unit))))
                    (Ir_lit (Lit_atom unit))))))))
         (Ir_tuple ((Ir_var ((path ()) (unique_name f_3) (source_name f)))))))))


================================================================================

Sample. To add a new test, copy and paste this above, and replace `_.ml` with
the right .ml file. Then remove the `#` that are preventing these commands from
running.

  $ # caramel compile --debug _.ml

  $ # cat _.ml.lambda

  $ # cat _.ml.ir


