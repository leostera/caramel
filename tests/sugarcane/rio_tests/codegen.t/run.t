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
    ((Ir_module ((path (Caramel.A)) (unique_name M1_17) (source_name M1))
       (Ir_let Exported ((path ()) (unique_name m1_f_14) (source_name m1_f))
         (Ir_fun (((path ()) (unique_name y_16) (source_name y)))
           (Ir_record
             (fields
               (((Ir_lit (Lit_atom first))
                  (Ir_var ((path ()) (unique_name y_16) (source_name y))))
                 ((Ir_lit (Lit_atom second))
                   (Ir_var ((path ()) (unique_name y_16) (source_name y))))))))
         (Ir_tuple
           ((Ir_var ((path ()) (unique_name m1_f_14) (source_name m1_f)))))))
      (Ir_module ((path ()) (unique_name Caramel.A) (source_name Caramel.A))
        (Ir_let Exported ((path ()) (unique_name f0_18) (source_name f0))
          (Ir_fun (((path ()) (unique_name param_20) (source_name param)))
            (Ir_ext_call (Caramel.A.M1 m1_f)
              ((Ir_ext_call (Caramel.A.M0 m0_f)
                 ((Ir_lit (Lit_int 1)) (Ir_lit (Lit_int 2)))))))
          (Ir_tuple
            ((Ir_var ((path ()) (unique_name M0_10) (source_name M0)))
              (Ir_var ((path ()) (unique_name M1_17) (source_name M1)))
              (Ir_var ((path ()) (unique_name f0_18) (source_name f0)))))))
      (Ir_module ((path (Caramel.A)) (unique_name M0_10) (source_name M0))
        (Ir_let Exported ((path ()) (unique_name m0_f_6) (source_name m0_f))
          (Ir_fun
            (((path ()) (unique_name first_8) (source_name first))
              ((path ()) (unique_name second_9) (source_name second)))
            (Ir_record
              (fields
                (((Ir_lit (Lit_atom first))
                   (Ir_var
                     ((path ()) (unique_name first_8) (source_name first))))
                  ((Ir_lit (Lit_atom second))
                    (Ir_var
                      ((path ()) (unique_name second_9) (source_name second))))))))
          (Ir_tuple
            ((Ir_var ((path ()) (unique_name m0_f_6) (source_name m0_f)))))))))

  $ cat Caramel.A.M0.core
  % Source code generated with Caramel.
  module 'Caramel.A.M0'
  [
   'm0_f'/2,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.A.M0') -| [])
  
  'module_info'/1 =
   (fun (Opts) -> call 'erlang':'get_module_info'('Caramel.A.M0', Opts) -| [])
  
  'm0_f'/2 =
   (fun (First, Second) -> ~{
                             'first' => First,
                             'second' => Second
                           }~ -| [])
  end
  

  $ erlc Caramel.A.M0.core

  $ cat Caramel.A.M1.core
  % Source code generated with Caramel.
  module 'Caramel.A.M1'
  [
   'm1_f'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.A.M1') -| [])
  
  'module_info'/1 =
   (fun (Opts) -> call 'erlang':'get_module_info'('Caramel.A.M1', Opts) -| [])
  
  'm1_f'/1 = (fun (Y) -> ~{
                           'first' => Y,
                           'second' => Y
                         }~ -| [])
  end
  

  $ erlc Caramel.A.M1.core

  $ cat Caramel.A.core
  % Source code generated with Caramel.
  module 'Caramel.A'
  [
   'f0'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.A') -| [])
  
  'module_info'/1 =
   (fun (Opts) -> call 'erlang':'get_module_info'('Caramel.A', Opts) -| [])
  
  'f0'/1 =
   (fun (Param) ->
   call 'Caramel.A.M1':'m1_f'(call 'Caramel.A.M0':'m0_f'(1, 2)) -| [])
  end
  

  $ erlc Caramel.A.core

================================================================================

  $ caramel compile --debug b.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (b.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
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
  caramel: [DEBUG] Writing Caramel.B.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat b.ml.lambda
  (let (main/4 = (function x/6 (io:format "Hello, ~p!" x/6)))
    (makeblock 0 main/4))

  $ cat b.ml.ir
  (Ir_program
    ((Ir_module ((path ()) (unique_name Caramel.B) (source_name Caramel.B))
       (Ir_let Exported ((path ()) (unique_name main_4) (source_name main))
         (Ir_fun (((path ()) (unique_name x_6) (source_name x)))
           (Ir_ext_call (io format)
             ((Ir_lit (Lit_string "Hello, ~p!"))
               (Ir_var ((path ()) (unique_name x_6) (source_name x))))))
         (Ir_tuple
           ((Ir_var ((path ()) (unique_name main_4) (source_name main)))))))))

  $ cat Caramel.B.core
  % Source code generated with Caramel.
  module 'Caramel.B'
  [
   'main'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.B') -| [])
  
  'module_info'/1 =
   (fun (Opts) -> call 'erlang':'get_module_info'('Caramel.B', Opts) -| [])
  
  'main'/1 =
   (fun (X) ->
   
     call 'io':'format'(
       #{
         #<72>(8,1,'integer',['unsigned'|['big']]),
         #<101>(8,1,'integer',['unsigned'|['big']]),
         #<108>(8,1,'integer',['unsigned'|['big']]),
         #<108>(8,1,'integer',['unsigned'|['big']]),
         #<111>(8,1,'integer',['unsigned'|['big']]),
         #<44>(8,1,'integer',['unsigned'|['big']]),
         #<32>(8,1,'integer',['unsigned'|['big']]),
         #<126>(8,1,'integer',['unsigned'|['big']]),
         #<112>(8,1,'integer',['unsigned'|['big']]),
         #<33>(8,1,'integer',['unsigned'|['big']])
       }#, X) -| [])
  end
  

  $ erlc Caramel.B.core

  $ escript Caramel.B.beam Joe
  Hello, "Joe"!


================================================================================

  $ caramel compile --debug c.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (c.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file c.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing c.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing c.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing c.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing c.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.C.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat c.ml.lambda

  $ cat c.ml.ir
  (Ir_program
    ((Ir_module ((path ()) (unique_name Caramel.C) (source_name Caramel.C))
       (Ir_letrec
         ((Exported ((path ()) (unique_name main_3) (source_name main))
            (Ir_fun (((path ()) (unique_name x_4) (source_name x)))
              (Ir_apply
                (Ir_fn_name ((path ()) (unique_name main_3) (source_name main))
                  1)
                ((Ir_var ((path ()) (unique_name x_4) (source_name x))))))))
         (Ir_tuple
           ((Ir_var ((path ()) (unique_name main_3) (source_name main)))))))))

  $ cat Caramel.C.core
  % Source code generated with Caramel.
  module 'Caramel.C'
  [
   'main'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.C') -| [])
  
  'module_info'/1 =
   (fun (Opts) -> call 'erlang':'get_module_info'('Caramel.C', Opts) -| [])
  
  'main'/1 = (fun (X) -> apply 'main'/1(X) -| [])
  end
  

  $ erlc Caramel.C.core

================================================================================

  $ caramel compile --debug d.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (d.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file d.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing d.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing d.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing d.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing d.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.D.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat d.ml.lambda
  (letrec
    (main/4
       (function x/5
         (if x/5
           (seq (erlang:display (field 0 x/5)) (apply main/4 (field 1 x/5))) 0)))
    (makeblock 0 main/4))

  $ cat d.ml.ir
  (Ir_program
    ((Ir_module ((path ()) (unique_name Caramel.D) (source_name Caramel.D))
       (Ir_letrec
         ((Exported ((path ()) (unique_name main_4) (source_name main))
            (Ir_fun (((path ()) (unique_name x_5) (source_name x)))
              (Ir_case (Ir_var ((path ()) (unique_name x_5) (source_name x)))
                ((P_nil (Ir_lit (Lit_atom unit)))
                  (P_ignore
                    (Ir_seq
                      (Ir_ext_call (erlang display)
                        ((Ir_ext_call (erlang hd)
                           ((Ir_var
                              ((path ()) (unique_name x_5) (source_name x)))))))
                      (Ir_apply
                        (Ir_fn_name
                          ((path ()) (unique_name main_4) (source_name main))
                          1)
                        ((Ir_ext_call (erlang tl)
                           ((Ir_var
                              ((path ()) (unique_name x_5) (source_name x))))))))))))))
         (Ir_tuple
           ((Ir_var ((path ()) (unique_name main_4) (source_name main)))))))))

  $ cat Caramel.D.core
  % Source code generated with Caramel.
  module 'Caramel.D'
  [
   'main'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.D') -| [])
  
  'module_info'/1 =
   (fun (Opts) -> call 'erlang':'get_module_info'('Caramel.D', Opts) -| [])
  
  'main'/1 =
   (fun (X) ->
   
     case X of
     <[]> when 'true' -> 'unit'
     <_> when 'true' ->
       do
         call 'erlang':'display'(call 'erlang':'hd'(X))
         apply 'main'/1(call 'erlang':'tl'(X))
     end -| [])
  end
  

  $ erlc Caramel.D.core

  $ escript Caramel.D.beam Joe Mike Robert
  "Joe"
  "Mike"
  "Robert"

================================================================================

Codegen for primitives

  $ caramel compile --debug primitives.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (primitives.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file primitives.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing primitives.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing primitives.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing primitives.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing primitives.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Primitives.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat primitives.ml.lambda
  (let
    (list0/3 = (function param/5 0)
     list1/6 = (function param/8 [0: 1 [0: 2 0]])
     list2/9 = (function param/11 [0: [0: 1 0] [0: [0: 2 0] 0]])
     list3/12 =
       (function param/14
         (makeblock 0 (apply list2/9 0)
           (makeblock 0 (makeblock 0 (apply list1/6 0) 0) 0)))
     nested/15 =
       (function param/23
         (makeblock 0
           (let (param/140 = 0)
             (makeblock 0 (apply list2/9 0)
               (makeblock 0
                 (makeblock 0 (let (param/139 = 0) (apply list0/3 0)) 0) 0)))
           0))
     unit/24 = (function param/26 0)
     tuple2/27 = (function param/29 (makeblock 0 (int,*) 1 (apply unit/24 0)))
     tuple3/30 =
       (function param/32 (makeblock 0 (int,*,int) 1 (apply tuple2/27 0) 3))
     tuple4/33 =
       (function param/35
         (makeblock 0 (int,int,*,int) 1 2 (apply tuple3/30 0) 4))
     tuple5/36 =
       (function param/38
         (makeblock 0 (int,int,int,*,int) 1 2 3 (apply tuple4/33 0) 5))
     polyvar0/39 = (function param/41 -899908020)
     polyvar1/42 =
       (function param/44 (makeblock 0 -910739347 (apply polyvar0/39 0)))
     polyvar2/45 =
       (function param/47
         (makeblock 0 -910739346
           (makeblock 0 -899908020 (apply polyvar1/42 0))))
     r0/57 = (function param/59 [0: 1])
     r1/60 = (function param/62 (makeblock 0 (int,*) 1 (apply r0/57 0)))
     r2/63 =
       (function param/65 (makeblock 0 (int,*,*) 1 "record" (apply r1/60 0)))
     f0/66 = (function param/68 : int (field 0 [0: 1]))
     f1/69 = (function param/71 : int (field 0 (apply r0/57 0)))
     f2/72 = (function r/74 : int (field 0 r/74))
     f3/75 = (function param/78 (field 2 param/78))
     v0/85 = (function param/87 0)
     v1/88 = (function param/90 [0: 1])
     v2/91 = (function param/93 [1: 1 2])
     vr/94 = (function param/96 [2: 0])
     gadt/99 = (function param/101 [0: "what"]))
    (makeblock 0 list0/3 list1/6 list2/9 list3/12 nested/15 unit/24 tuple2/27
      tuple3/30 tuple4/33 tuple5/36 polyvar0/39 polyvar1/42 polyvar2/45 r0/57
      r1/60 r2/63 f0/66 f1/69 f2/72 f3/75 v0/85 v1/88 v2/91 vr/94 gadt/99))

  $ cat primitives.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Primitives)
         (source_name Caramel.Primitives))
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
               ((path ()) (unique_name list3_12) (source_name list3))
               (Ir_fun (((path ()) (unique_name param_14) (source_name param)))
                 (Ir_cons
                   (Ir_apply
                     (Ir_fn_name
                       ((path ()) (unique_name list2_9) (source_name list2)) 1)
                     ((Ir_lit (Lit_atom unit))))
                   (Ir_cons
                     (Ir_cons
                       (Ir_apply
                         (Ir_fn_name
                           ((path ()) (unique_name list1_6)
                             (source_name list1))
                           1)
                         ((Ir_lit (Lit_atom unit))))
                       Ir_nil)
                     Ir_nil)))
               (Ir_let Exported
                 ((path ()) (unique_name nested_15) (source_name nested))
                 (Ir_fun
                   (((path ()) (unique_name param_23) (source_name param)))
                   (Ir_cons
                     (Ir_let Private
                       ((path ()) (unique_name param_140) (source_name param))
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
                               ((path ()) (unique_name param_139)
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
                 (Ir_let Exported
                   ((path ()) (unique_name unit_24) (source_name unit))
                   (Ir_fun
                     (((path ()) (unique_name param_26) (source_name param)))
                     (Ir_lit (Lit_atom unit)))
                   (Ir_let Exported
                     ((path ()) (unique_name tuple2_27) (source_name tuple2))
                     (Ir_fun
                       (((path ()) (unique_name param_29) (source_name param)))
                       (Ir_tuple
                         ((Ir_lit (Lit_int 1))
                           (Ir_apply
                             (Ir_fn_name
                               ((path ()) (unique_name unit_24)
                                 (source_name unit))
                               1)
                             ((Ir_lit (Lit_atom unit)))))))
                     (Ir_let Exported
                       ((path ()) (unique_name tuple3_30) (source_name tuple3))
                       (Ir_fun
                         (((path ()) (unique_name param_32)
                            (source_name param)))
                         (Ir_tuple
                           ((Ir_lit (Lit_int 1))
                             (Ir_apply
                               (Ir_fn_name
                                 ((path ()) (unique_name tuple2_27)
                                   (source_name tuple2))
                                 1)
                               ((Ir_lit (Lit_atom unit))))
                             (Ir_lit (Lit_int 3)))))
                       (Ir_let Exported
                         ((path ()) (unique_name tuple4_33)
                           (source_name tuple4))
                         (Ir_fun
                           (((path ()) (unique_name param_35)
                              (source_name param)))
                           (Ir_tuple
                             ((Ir_lit (Lit_int 1)) (Ir_lit (Lit_int 2))
                               (Ir_apply
                                 (Ir_fn_name
                                   ((path ()) (unique_name tuple3_30)
                                     (source_name tuple3))
                                   1)
                                 ((Ir_lit (Lit_atom unit))))
                               (Ir_lit (Lit_int 4)))))
                         (Ir_let Exported
                           ((path ()) (unique_name tuple5_36)
                             (source_name tuple5))
                           (Ir_fun
                             (((path ()) (unique_name param_38)
                                (source_name param)))
                             (Ir_tuple
                               ((Ir_lit (Lit_int 1)) (Ir_lit (Lit_int 2))
                                 (Ir_lit (Lit_int 3))
                                 (Ir_apply
                                   (Ir_fn_name
                                     ((path ()) (unique_name tuple4_33)
                                       (source_name tuple4))
                                     1)
                                   ((Ir_lit (Lit_atom unit))))
                                 (Ir_lit (Lit_int 5)))))
                           (Ir_let Exported
                             ((path ()) (unique_name polyvar0_39)
                               (source_name polyvar0))
                             (Ir_fun
                               (((path ()) (unique_name param_41)
                                  (source_name param)))
                               (Ir_lit (Lit_atom poly)))
                             (Ir_let Exported
                               ((path ()) (unique_name polyvar1_42)
                                 (source_name polyvar1))
                               (Ir_fun
                                 (((path ()) (unique_name param_44)
                                    (source_name param)))
                                 (Ir_tuple
                                   ((Ir_lit (Lit_atom what1))
                                     (Ir_lit (Lit_int -910739347))
                                     (Ir_apply
                                       (Ir_fn_name
                                         ((path ()) (unique_name polyvar0_39)
                                           (source_name polyvar0))
                                         1)
                                       ((Ir_lit (Lit_atom unit)))))))
                               (Ir_let Exported
                                 ((path ()) (unique_name polyvar2_45)
                                   (source_name polyvar2))
                                 (Ir_fun
                                   (((path ()) (unique_name param_47)
                                      (source_name param)))
                                   (Ir_tuple
                                     ((Ir_lit (Lit_atom what2))
                                       (Ir_lit (Lit_int -910739346))
                                       (Ir_tuple
                                         ((Ir_lit (Lit_atom poly))
                                           (Ir_apply
                                             (Ir_fn_name
                                               ((path ())
                                                 (unique_name polyvar1_42)
                                                 (source_name polyvar1))
                                               1)
                                             ((Ir_lit (Lit_atom unit)))))))))
                                 (Ir_let Exported
                                   ((path ()) (unique_name r0_57)
                                     (source_name r0))
                                   (Ir_fun
                                     (((path ()) (unique_name param_59)
                                        (source_name param)))
                                     (Ir_record
                                       (fields
                                         (((Ir_lit (Lit_string _0))
                                            (Ir_lit (Lit_int 1)))))))
                                   (Ir_let Exported
                                     ((path ()) (unique_name r1_60)
                                       (source_name r1))
                                     (Ir_fun
                                       (((path ()) (unique_name param_62)
                                          (source_name param)))
                                       (Ir_record
                                         (fields
                                           (((Ir_lit (Lit_atom _0))
                                              (Ir_lit (Lit_int 1)))
                                             ((Ir_lit (Lit_atom _1))
                                               (Ir_apply
                                                 (Ir_fn_name
                                                   ((path ())
                                                     (unique_name r0_57)
                                                     (source_name r0))
                                                   1)
                                                 ((Ir_lit (Lit_atom unit)))))))))
                                     (Ir_let Exported
                                       ((path ()) (unique_name r2_63)
                                         (source_name r2))
                                       (Ir_fun
                                         (((path ()) (unique_name param_65)
                                            (source_name param)))
                                         (Ir_record
                                           (fields
                                             (((Ir_lit (Lit_atom _0))
                                                (Ir_lit (Lit_int 1)))
                                               ((Ir_lit (Lit_atom _1))
                                                 (Ir_lit (Lit_string record)))
                                               ((Ir_lit (Lit_atom _2))
                                                 (Ir_apply
                                                   (Ir_fn_name
                                                     ((path ())
                                                       (unique_name r1_60)
                                                       (source_name r1))
                                                     1)
                                                   ((Ir_lit (Lit_atom unit)))))))))
                                       (Ir_let Exported
                                         ((path ()) (unique_name f0_66)
                                           (source_name f0))
                                         (Ir_fun
                                           (((path ()) (unique_name param_68)
                                              (source_name param)))
                                           (Ir_field 0
                                             ((Ir_lit (Lit_string _0)))
                                             (Ir_record
                                               (fields
                                                 (((Ir_lit (Lit_string _0))
                                                    (Ir_lit (Lit_int 1))))))))
                                         (Ir_let Exported
                                           ((path ()) (unique_name f1_69)
                                             (source_name f1))
                                           (Ir_fun
                                             (((path ()) (unique_name param_71)
                                                (source_name param)))
                                             (Ir_field 0
                                               ((Ir_lit (Lit_string _0)))
                                               (Ir_apply
                                                 (Ir_fn_name
                                                   ((path ())
                                                     (unique_name r0_57)
                                                     (source_name r0))
                                                   1)
                                                 ((Ir_lit (Lit_atom unit))))))
                                           (Ir_let Exported
                                             ((path ()) (unique_name f2_72)
                                               (source_name f2))
                                             (Ir_fun
                                               (((path ()) (unique_name r_74)
                                                  (source_name r)))
                                               (Ir_field 0
                                                 ((Ir_lit (Lit_string _0)))
                                                 (Ir_var
                                                   ((path ())
                                                     (unique_name r_74)
                                                     (source_name r)))))
                                             (Ir_let Exported
                                               ((path ()) (unique_name f3_75)
                                                 (source_name f3))
                                               (Ir_fun
                                                 (((path ())
                                                    (unique_name param_78)
                                                    (source_name param)))
                                                 (Ir_ext_call (erlang element)
                                                   ((Ir_lit (Lit_int 3))
                                                     (Ir_var
                                                       ((path ())
                                                         (unique_name param_78)
                                                         (source_name param))))))
                                               (Ir_let Exported
                                                 ((path ()) (unique_name v0_85)
                                                   (source_name v0))
                                                 (Ir_fun
                                                   (((path ())
                                                      (unique_name param_87)
                                                      (source_name param)))
                                                   (Ir_lit (Lit_atom v)))
                                                 (Ir_let Exported
                                                   ((path ())
                                                     (unique_name v1_88)
                                                     (source_name v1))
                                                   (Ir_fun
                                                     (((path ())
                                                        (unique_name param_90)
                                                        (source_name param)))
                                                     (Ir_tuple
                                                       ((Ir_lit (Lit_atom v1))
                                                         (Ir_lit (Lit_int 1)))))
                                                   (Ir_let Exported
                                                     ((path ())
                                                       (unique_name v2_91)
                                                       (source_name v2))
                                                     (Ir_fun
                                                       (((path ())
                                                          (unique_name
                                                            param_93)
                                                          (source_name param)))
                                                       (Ir_tuple
                                                         ((Ir_lit
                                                            (Lit_atom v2))
                                                           (Ir_lit (Lit_int 1))
                                                           (Ir_lit (Lit_int 2)))))
                                                     (Ir_let Exported
                                                       ((path ())
                                                         (unique_name vr_94)
                                                         (source_name vr))
                                                       (Ir_fun
                                                         (((path ())
                                                            (unique_name
                                                              param_96)
                                                            (source_name param)))
                                                         (Ir_record
                                                           (fields
                                                             (((Ir_lit
                                                                 (Lit_string
                                                                   field))
                                                                (Ir_lit
                                                                  (Lit_atom
                                                                    false)))))))
                                                       (Ir_let Exported
                                                         ((path ())
                                                           (unique_name
                                                             gadt_99)
                                                           (source_name gadt))
                                                         (Ir_fun
                                                           (((path ())
                                                              (unique_name
                                                                param_101)
                                                              (source_name
                                                                param)))
                                                           (Ir_tuple
                                                             ((Ir_lit
                                                                (Lit_atom
                                                                  hello))
                                                               (Ir_lit
                                                                 (Lit_string
                                                                   what)))))
                                                         (Ir_tuple
                                                           ((Ir_var
                                                              ((path ())
                                                                (unique_name
                                                                  list0_3)
                                                                (source_name
                                                                  list0)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   list1_6)
                                                                 (source_name
                                                                   list1)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   list2_9)
                                                                 (source_name
                                                                   list2)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   list3_12)
                                                                 (source_name
                                                                   list3)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   nested_15)
                                                                 (source_name
                                                                   nested)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   unit_24)
                                                                 (source_name
                                                                   unit)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   tuple2_27)
                                                                 (source_name
                                                                   tuple2)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   tuple3_30)
                                                                 (source_name
                                                                   tuple3)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   tuple4_33)
                                                                 (source_name
                                                                   tuple4)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   tuple5_36)
                                                                 (source_name
                                                                   tuple5)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   polyvar0_39)
                                                                 (source_name
                                                                   polyvar0)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   polyvar1_42)
                                                                 (source_name
                                                                   polyvar1)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   polyvar2_45)
                                                                 (source_name
                                                                   polyvar2)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   r0_57)
                                                                 (source_name
                                                                   r0)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   r1_60)
                                                                 (source_name
                                                                   r1)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   r2_63)
                                                                 (source_name
                                                                   r2)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   f0_66)
                                                                 (source_name
                                                                   f0)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   f1_69)
                                                                 (source_name
                                                                   f1)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   f2_72)
                                                                 (source_name
                                                                   f2)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   f3_75)
                                                                 (source_name
                                                                   f3)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   v0_85)
                                                                 (source_name
                                                                   v0)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   v1_88)
                                                                 (source_name
                                                                   v1)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   v2_91)
                                                                 (source_name
                                                                   v2)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   vr_94)
                                                                 (source_name
                                                                   vr)))
                                                             (Ir_var
                                                               ((path ())
                                                                 (unique_name
                                                                   gadt_99)
                                                                 (source_name
                                                                   gadt)))))))))))))))))))))))))))))))))

  $ cat Caramel.Primitives.core
  % Source code generated with Caramel.
  module 'Caramel.Primitives'
  [
   'gadt'/1,
   'vr'/1,
   'v2'/1,
   'v1'/1,
   'v0'/1,
   'f3'/1,
   'f2'/1,
   'f1'/1,
   'f0'/1,
   'r2'/1,
   'r1'/1,
   'r0'/1,
   'polyvar2'/1,
   'polyvar1'/1,
   'polyvar0'/1,
   'tuple5'/1,
   'tuple4'/1,
   'tuple3'/1,
   'tuple2'/1,
   'unit'/1,
   'nested'/1,
   'list3'/1,
   'list2'/1,
   'list1'/1,
   'list0'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.Primitives') -| [])
  
  'module_info'/1 =
   (fun (Opts) ->
   call 'erlang':'get_module_info'('Caramel.Primitives', Opts) -| [])
  
  'gadt'/1 =
   (fun (Param) ->
   
     {'hello',
      #{
        #<119>(8,1,'integer',['unsigned'|['big']]),
        #<104>(8,1,'integer',['unsigned'|['big']]),
        #<97>(8,1,'integer',['unsigned'|['big']]),
        #<116>(8,1,'integer',['unsigned'|['big']])
      }#} -| [])
  
  'vr'/1 =
   (fun (Param) ->
   
     ~{
       #{
         #<102>(8,1,'integer',['unsigned'|['big']]),
         #<105>(8,1,'integer',['unsigned'|['big']]),
         #<101>(8,1,'integer',['unsigned'|['big']]),
         #<108>(8,1,'integer',['unsigned'|['big']]),
         #<100>(8,1,'integer',['unsigned'|['big']])
       }# => 'false'
     }~ -| [])
  
  'v2'/1 = (fun (Param) -> {'v2', 1, 2} -| [])
  
  'v1'/1 = (fun (Param) -> {'v1', 1} -| [])
  
  'v0'/1 = (fun (Param) -> 'v' -| [])
  
  'f3'/1 = (fun (Param) -> call 'erlang':'element'(3, Param) -| [])
  
  'f2'/1 =
   (fun (R) ->
   
     call 'maps':'get'(
       #{
         #<95>(8,1,'integer',['unsigned'|['big']]),
         #<48>(8,1,'integer',['unsigned'|['big']])
       }#, R) -| [])
  
  'f1'/1 =
   (fun (Param) ->
   
     call 'maps':'get'(
       #{
         #<95>(8,1,'integer',['unsigned'|['big']]),
         #<48>(8,1,'integer',['unsigned'|['big']])
       }#, apply 'r0'/1('unit')) -| [])
  
  'f0'/1 =
   (fun (Param) ->
   
     call 'maps':'get'(
       #{
         #<95>(8,1,'integer',['unsigned'|['big']]),
         #<48>(8,1,'integer',['unsigned'|['big']])
       }#,
       ~{
         #{
           #<95>(8,1,'integer',['unsigned'|['big']]),
           #<48>(8,1,'integer',['unsigned'|['big']])
         }# => 1
       }~) -| [])
  
  'r2'/1 =
   (fun (Param) ->
   
     ~{
       '_0' => 1,
       '_1' => #{
                 #<114>(8,1,'integer',['unsigned'|['big']]),
                 #<101>(8,1,'integer',['unsigned'|['big']]),
                 #<99>(8,1,'integer',['unsigned'|['big']]),
                 #<111>(8,1,'integer',['unsigned'|['big']]),
                 #<114>(8,1,'integer',['unsigned'|['big']]),
                 #<100>(8,1,'integer',['unsigned'|['big']])
               }#,
       '_2' => apply 'r1'/1('unit')
     }~ -| [])
  
  'r1'/1 = (fun (Param) -> ~{
                             '_0' => 1,
                             '_1' => apply 'r0'/1('unit')
                           }~ -| [])
  
  'r0'/1 =
   (fun (Param) ->
   
     ~{
       #{
         #<95>(8,1,'integer',['unsigned'|['big']]),
         #<48>(8,1,'integer',['unsigned'|['big']])
       }# => 1
     }~ -| [])
  
  'polyvar2'/1 =
   (fun (Param) ->
   {'what2', -910739346, {'poly', apply 'polyvar1'/1('unit')}} -| [])
  
  'polyvar1'/1 =
   (fun (Param) -> {'what1', -910739347, apply 'polyvar0'/1('unit')} -| [])
  
  'polyvar0'/1 = (fun (Param) -> 'poly' -| [])
  
  'tuple5'/1 = (fun (Param) -> {1, 2, 3, apply 'tuple4'/1('unit'), 5} -| [])
  
  'tuple4'/1 = (fun (Param) -> {1, 2, apply 'tuple3'/1('unit'), 4} -| [])
  
  'tuple3'/1 = (fun (Param) -> {1, apply 'tuple2'/1('unit'), 3} -| [])
  
  'tuple2'/1 = (fun (Param) -> {1, apply 'unit'/1('unit')} -| [])
  
  'unit'/1 = (fun (Param) -> 'unit' -| [])
  
  'nested'/1 =
   (fun (Param) ->
   
     [
      let <Param> = 'unit' in
      [ apply 'list2'/1('unit')|
       [ [ let <Param> = 'unit' in apply 'list0'/1('unit')|[] ]|[] ] ]|
      [] ] -| [])
  
  'list3'/1 =
   (fun (Param) ->
   [ apply 'list2'/1('unit')|[ [ apply 'list1'/1('unit')|[] ]|[] ] ] -| [])
  
  'list2'/1 = (fun (Param) -> [ [ 1|[] ]|[ [ 2|[] ]|[] ] ] -| [])
  
  'list1'/1 = (fun (Param) -> [ 1|[ 2|[] ] ] -| [])
  
  'list0'/1 = (fun (Param) -> [] -| [])
  end
  

  $ erlc Caramel.Primitives.core

================================================================================

Sample. To add a new test, copy and paste this above, and replace `_.ml` with
the right .ml file. Then remove the `#` that are preventing these commands from
running.

  $ # caramel compile --debug _.ml

  $ # cat _.ml.lambda

  $ # cat _.ml.ir


