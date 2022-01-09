================================================================================

Support for lists.

  $ caramel compile --debug prim_list.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (prim_list.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file prim_list.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing prim_list.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing prim_list.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing prim_list.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing prim_list.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Prim_list.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat prim_list.ml.lambda
  (let
    (list0/3 = (function param/5 0)
     list1/6 = (function param/8 [0: 1 [0: 2 0]])
     list2/9 = (function param/11 [0: [0: 1 0] [0: [0: 2 0] 0]])
     nested/12 =
       (function param/20
         (makeblock 0
           (let (param/26 = 0)
             (makeblock 0 (apply list2/9 0)
               (makeblock 0
                 (makeblock 0 (let (param/25 = 0) (apply list0/3 0)) 0) 0)))
           0)))

  $ cat prim_list.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Prim_list)
         (source_name Caramel.Prim_list))
       (Ir_let Exported ((path ()) (unique_name list0_3) (source_name list0))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           Ir_nil)
         (Ir_let Exported ((path ()) (unique_name list1_6) (source_name list1))
           (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
             (Ir_cons (Ir_lit (Lit_int 1))
               (Ir_cons (Ir_lit (Lit_int 2)) Ir_nil)))
           (Ir_let Exported
             ((path ()) (unique_name list2_9) (source_name list2))
             (Ir_fun (((path ()) (unique_name param_11) (source_name param)))
               (Ir_cons (Ir_cons (Ir_lit (Lit_int 1)) Ir_nil)
                 (Ir_cons (Ir_cons (Ir_lit (Lit_int 2)) Ir_nil) Ir_nil)))
             (Ir_let Exported
               ((path ()) (unique_name nested_12) (source_name nested))
               (Ir_fun (((path ()) (unique_name param_20) (source_name param)))
                 (Ir_cons
                   (Ir_let Private
                     ((path ()) (unique_name param_26) (source_name param))
                     (Ir_lit (Lit_atom unit))
                     (Ir_cons
                       (Ir_apply
                         (Ir_fn_name
                           ((path ()) (unique_name list2_9)
                             (source_name list2))
                           1)
                         ((Ir_lit (Lit_atom unit))))
                       (Ir_cons
                         (Ir_cons
                           (Ir_let Private
                             ((path ()) (unique_name param_25)
                               (source_name param))
                             (Ir_lit (Lit_atom unit))
                             (Ir_apply
                               (Ir_fn_name
                                 ((path ()) (unique_name list0_3)
                                   (source_name list0))
                                 1)
                               ((Ir_lit (Lit_atom unit)))))
                           Ir_nil)
                         Ir_nil)))
                   Ir_nil))
               (Ir_tuple
                 ((Ir_var
                    ((path ()) (unique_name list0_3) (source_name list0)))
                   (Ir_var
                     ((path ()) (unique_name list1_6) (source_name list1)))
                   (Ir_var
                     ((path ()) (unique_name list2_9) (source_name list2)))
                   (Ir_var
                     ((path ()) (unique_name nested_12) (source_name nested))))))))))))

================================================================================

Support for polymorphic variants.

  $ caramel compile --debug prim_polyvar.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (prim_polyvar.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file prim_polyvar.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing prim_polyvar.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing prim_polyvar.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing prim_polyvar.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing prim_polyvar.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Prim_polyvar.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat prim_polyvar.ml.lambda
  (let
    (polyvar0/3 = (function param/5 -899908020)
     polyvar1/6 =
       (function param/8 (makeblock 0 -910739347 (apply polyvar0/3 0)))
     polyvar2/9 =
       (function param/11
         (makeblock 0 -910739346 (makeblock 0 -899908020 (apply polyvar1/6 0)))))
    (makeblock 0 polyvar0/3 polyvar1/6 polyvar2/9))

  $ cat prim_polyvar.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Prim_polyvar)
         (source_name Caramel.Prim_polyvar))
       (Ir_let Exported
         ((path ()) (unique_name polyvar0_3) (source_name polyvar0))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_atom poly)))
         (Ir_let Exported
           ((path ()) (unique_name polyvar1_6) (source_name polyvar1))
           (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
             (Ir_tuple
               ((Ir_lit (Lit_atom what1)) (Ir_lit (Lit_int -910739347))
                 (Ir_apply
                   (Ir_fn_name
                     ((path ()) (unique_name polyvar0_3)
                       (source_name polyvar0))
                     1)
                   ((Ir_lit (Lit_atom unit)))))))
           (Ir_let Exported
             ((path ()) (unique_name polyvar2_9) (source_name polyvar2))
             (Ir_fun (((path ()) (unique_name param_11) (source_name param)))
               (Ir_tuple
                 ((Ir_lit (Lit_atom what2)) (Ir_lit (Lit_int -910739346))
                   (Ir_tuple
                     ((Ir_lit (Lit_atom poly))
                       (Ir_apply
                         (Ir_fn_name
                           ((path ()) (unique_name polyvar1_6)
                             (source_name polyvar1))
                           1)
                         ((Ir_lit (Lit_atom unit)))))))))
             (Ir_tuple
               ((Ir_var
                  ((path ()) (unique_name polyvar0_3) (source_name polyvar0)))
                 (Ir_var
                   ((path ()) (unique_name polyvar1_6) (source_name polyvar1)))
                 (Ir_var
                   ((path ()) (unique_name polyvar2_9) (source_name polyvar2)))))))))))

================================================================================

Support for variants.

  $ caramel compile --debug prim_variant.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (prim_variant.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file prim_variant.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing prim_variant.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing prim_variant.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing prim_variant.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing prim_variant.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Prim_variant.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat prim_variant.ml.lambda
  (let
    (v0/9 = (function param/11 0)
     v1/12 = (function param/14 [0: 1])
     v2/15 = (function param/17 [1: 1 2])
     vr/18 = (function param/20 [2: 0])
     gadt/23 = (function param/25 [0: "what"]))
    (makeblock 0 v0/9 v1/12 v2/15 vr/18 gadt/23))

  $ cat prim_variant.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Prim_variant)
         (source_name Caramel.Prim_variant))
       (Ir_let Exported ((path ()) (unique_name v0_9) (source_name v0))
         (Ir_fun (((path ()) (unique_name param_11) (source_name param)))
           (Ir_lit (Lit_atom v)))
         (Ir_let Exported ((path ()) (unique_name v1_12) (source_name v1))
           (Ir_fun (((path ()) (unique_name param_14) (source_name param)))
             (Ir_tuple ((Ir_lit (Lit_atom v1)) (Ir_lit (Lit_int 1)))))
           (Ir_let Exported ((path ()) (unique_name v2_15) (source_name v2))
             (Ir_fun (((path ()) (unique_name param_17) (source_name param)))
               (Ir_tuple
                 ((Ir_lit (Lit_atom v2)) (Ir_lit (Lit_int 1))
                   (Ir_lit (Lit_int 2)))))
             (Ir_let Exported ((path ()) (unique_name vr_18) (source_name vr))
               (Ir_fun (((path ()) (unique_name param_20) (source_name param)))
                 (Ir_record
                   (fields
                     (((Ir_lit (Lit_string field)) (Ir_lit (Lit_atom false)))))))
               (Ir_let Exported
                 ((path ()) (unique_name gadt_23) (source_name gadt))
                 (Ir_fun
                   (((path ()) (unique_name param_25) (source_name param)))
                   (Ir_tuple
                     ((Ir_lit (Lit_atom hello)) (Ir_lit (Lit_string what)))))
                 (Ir_tuple
                   ((Ir_var ((path ()) (unique_name v0_9) (source_name v0)))
                     (Ir_var ((path ()) (unique_name v1_12) (source_name v1)))
                     (Ir_var ((path ()) (unique_name v2_15) (source_name v2)))
                     (Ir_var ((path ()) (unique_name vr_18) (source_name vr)))
                     (Ir_var
                       ((path ()) (unique_name gadt_23) (source_name gadt)))))))))))))

================================================================================

Support for records.

  $ caramel compile --debug prim_record.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (prim_record.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file prim_record.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing prim_record.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing prim_record.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing prim_record.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing prim_record.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Prim_record.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat prim_record.ml.lambda
  (let
    (r0/12 = (function param/14 [0: 1])
     r1/15 = (function param/17 (makeblock 0 (int,*) 1 (apply r0/12 0)))
     r2/18 =
       (function param/20 (makeblock 0 (int,*,*) 1 "record" (apply r1/15 0)))
     f0/21 = (function param/23 : int (field 0 [0: 1]))
     f1/24 = (function param/26 : int (field 0 (apply r0/12 0)))
     f2/27 = (function r/29 : int (field 0 r/29))
     f3/30 = (function param/33 (field 2 param/33)))
    (makeblock 0 r0/12 r1/15 r2/18 f0/21 f1/24 f2/27 f3/30))

  $ cat prim_record.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Prim_record)
         (source_name Caramel.Prim_record))
       (Ir_let Exported ((path ()) (unique_name r0_12) (source_name r0))
         (Ir_fun (((path ()) (unique_name param_14) (source_name param)))
           (Ir_record
             (fields (((Ir_lit (Lit_string _0)) (Ir_lit (Lit_int 1)))))))
         (Ir_let Exported ((path ()) (unique_name r1_15) (source_name r1))
           (Ir_fun (((path ()) (unique_name param_17) (source_name param)))
             (Ir_record
               (fields
                 (((Ir_lit (Lit_atom _0)) (Ir_lit (Lit_int 1)))
                   ((Ir_lit (Lit_atom _1))
                     (Ir_apply
                       (Ir_fn_name
                         ((path ()) (unique_name r0_12) (source_name r0)) 1)
                       ((Ir_lit (Lit_atom unit)))))))))
           (Ir_let Exported ((path ()) (unique_name r2_18) (source_name r2))
             (Ir_fun (((path ()) (unique_name param_20) (source_name param)))
               (Ir_record
                 (fields
                   (((Ir_lit (Lit_atom _0)) (Ir_lit (Lit_int 1)))
                     ((Ir_lit (Lit_atom _1)) (Ir_lit (Lit_string record)))
                     ((Ir_lit (Lit_atom _2))
                       (Ir_apply
                         (Ir_fn_name
                           ((path ()) (unique_name r1_15) (source_name r1)) 1)
                         ((Ir_lit (Lit_atom unit)))))))))
             (Ir_let Exported ((path ()) (unique_name f0_21) (source_name f0))
               (Ir_fun (((path ()) (unique_name param_23) (source_name param)))
                 (Ir_field 0 ((Ir_lit (Lit_string _0)))
                   (Ir_record
                     (fields (((Ir_lit (Lit_string _0)) (Ir_lit (Lit_int 1))))))))
               (Ir_let Exported
                 ((path ()) (unique_name f1_24) (source_name f1))
                 (Ir_fun
                   (((path ()) (unique_name param_26) (source_name param)))
                   (Ir_field 0 ((Ir_lit (Lit_string _0)))
                     (Ir_apply
                       (Ir_fn_name
                         ((path ()) (unique_name r0_12) (source_name r0)) 1)
                       ((Ir_lit (Lit_atom unit))))))
                 (Ir_let Exported
                   ((path ()) (unique_name f2_27) (source_name f2))
                   (Ir_fun (((path ()) (unique_name r_29) (source_name r)))
                     (Ir_field 0 ((Ir_lit (Lit_string _0)))
                       (Ir_var ((path ()) (unique_name r_29) (source_name r)))))
                   (Ir_let Exported
                     ((path ()) (unique_name f3_30) (source_name f3))
                     (Ir_fun
                       (((path ()) (unique_name param_33) (source_name param)))
                       (Ir_ext_call (erlang element)
                         ((Ir_lit (Lit_int 3))
                           (Ir_var
                             ((path ()) (unique_name param_33)
                               (source_name param))))))
                     (Ir_tuple
                       ((Ir_var
                          ((path ()) (unique_name r0_12) (source_name r0)))
                         (Ir_var
                           ((path ()) (unique_name r1_15) (source_name r1)))
                         (Ir_var
                           ((path ()) (unique_name r2_18) (source_name r2)))
                         (Ir_var
                           ((path ()) (unique_name f0_21) (source_name f0)))
                         (Ir_var
                           ((path ()) (unique_name f1_24) (source_name f1)))
                         (Ir_var
                           ((path ()) (unique_name f2_27) (source_name f2)))
                         (Ir_var
                           ((path ()) (unique_name f3_30) (source_name f3)))))))))))))))

================================================================================

Support for tuples.

  $ caramel compile --debug prim_tuple.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (prim_tuple.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file prim_tuple.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing prim_tuple.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing prim_tuple.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing prim_tuple.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing prim_tuple.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Prim_tuple.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat prim_tuple.ml.lambda
  (let
    (unit/3 = (function param/5 0)
     tuple2/6 = (function param/8 (makeblock 0 (int,*) 1 (apply unit/3 0)))
     tuple3/9 =
       (function param/11 (makeblock 0 (int,*,int) 1 (apply tuple2/6 0) 3))
     tuple4/12 =
       (function param/14
         (makeblock 0 (int,int,*,int) 1 2 (apply tuple3/9 0) 4))
     tuple5/15 =
       (function param/17
         (makeblock 0 (int,int,int,*,int) 1 2 3 (apply tuple4/12 0) 5)))
    (makeblock 0 unit/3 tuple2/6 tuple3/9 tuple4/12 tuple5/15))

  $ cat prim_tuple.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Prim_tuple)
         (source_name Caramel.Prim_tuple))
       (Ir_let Exported ((path ()) (unique_name unit_3) (source_name unit))
         (Ir_fun (((path ()) (unique_name param_5) (source_name param)))
           (Ir_lit (Lit_atom unit)))
         (Ir_let Exported
           ((path ()) (unique_name tuple2_6) (source_name tuple2))
           (Ir_fun (((path ()) (unique_name param_8) (source_name param)))
             (Ir_tuple
               ((Ir_lit (Lit_int 1))
                 (Ir_apply
                   (Ir_fn_name
                     ((path ()) (unique_name unit_3) (source_name unit)) 1)
                   ((Ir_lit (Lit_atom unit)))))))
           (Ir_let Exported
             ((path ()) (unique_name tuple3_9) (source_name tuple3))
             (Ir_fun (((path ()) (unique_name param_11) (source_name param)))
               (Ir_tuple
                 ((Ir_lit (Lit_int 1))
                   (Ir_apply
                     (Ir_fn_name
                       ((path ()) (unique_name tuple2_6) (source_name tuple2))
                       1)
                     ((Ir_lit (Lit_atom unit))))
                   (Ir_lit (Lit_int 3)))))
             (Ir_let Exported
               ((path ()) (unique_name tuple4_12) (source_name tuple4))
               (Ir_fun (((path ()) (unique_name param_14) (source_name param)))
                 (Ir_tuple
                   ((Ir_lit (Lit_int 1)) (Ir_lit (Lit_int 2))
                     (Ir_apply
                       (Ir_fn_name
                         ((path ()) (unique_name tuple3_9)
                           (source_name tuple3))
                         1)
                       ((Ir_lit (Lit_atom unit))))
                     (Ir_lit (Lit_int 4)))))
               (Ir_let Exported
                 ((path ()) (unique_name tuple5_15) (source_name tuple5))
                 (Ir_fun
                   (((path ()) (unique_name param_17) (source_name param)))
                   (Ir_tuple
                     ((Ir_lit (Lit_int 1)) (Ir_lit (Lit_int 2))
                       (Ir_lit (Lit_int 3))
                       (Ir_apply
                         (Ir_fn_name
                           ((path ()) (unique_name tuple4_12)
                             (source_name tuple4))
                           1)
                         ((Ir_lit (Lit_atom unit))))
                       (Ir_lit (Lit_int 5)))))
                 (Ir_tuple
                   ((Ir_var
                      ((path ()) (unique_name unit_3) (source_name unit)))
                     (Ir_var
                       ((path ()) (unique_name tuple2_6) (source_name tuple2)))
                     (Ir_var
                       ((path ()) (unique_name tuple3_9) (source_name tuple3)))
                     (Ir_var
                       ((path ()) (unique_name tuple4_12) (source_name tuple4)))
                     (Ir_var
                       ((path ()) (unique_name tuple5_15) (source_name tuple5)))))))))))))
