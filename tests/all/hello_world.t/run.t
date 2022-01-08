Simple hello world test.

  $ caramel compile --debug hello_world.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (hello_world.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file hello_world.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing hello_world.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing hello_world.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing hello_world.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing hello_world.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Hello_world.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ diff Caramel.Hello_world.core Caramel.Hello_world.core.expected
  2c2
  < module 'Caramel.Hello_world'
  ---
  > module 'Hello_world'
  4,6c4
  <  'main'/1,
  <  'module_info'/0,
  <  'module_info'/1
  ---
  >  'main'/1
  10,16d7
  < 'module_info'/0 =
  <  (fun () -> call 'erlang':'get_module_info'('Caramel.Hello_world') -| [])
  < 
  < 'module_info'/1 =
  <  (fun (Opts) ->
  <  call 'erlang':'get_module_info'('Caramel.Hello_world', Opts) -| [])
  < 
  51c42
  <         apply 'main'/1(call 'erlang':'tl'(X))
  ---
  >        apply 'main'/1(call 'erlang':'tl'(X))
  [1]

  $ cat hello_world.ml.lambda
  (letrec
    (main/4
       (function x/5
         (if x/5
           (seq (io:format "Hello, ~s!\n" (makeblock 0 (field 0 x/5) 0))
             (apply main/4 (field 1 x/5)))
           (io:format "~s\n" [0: "bye!" 0]))))

  $ cat hello_world.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Hello_world)
         (source_name Caramel.Hello_world))
       (Ir_letrec
         ((Exported ((path ()) (unique_name main_4) (source_name main))
            (Ir_fun (((path ()) (unique_name x_5) (source_name x)))
              (Ir_case (Ir_var ((path ()) (unique_name x_5) (source_name x)))
                ((P_nil
                   (Ir_ext_call (io format)
                     ((Ir_lit (Lit_string "~s\n"))
                       (Ir_cons (Ir_lit (Lit_string bye!)) Ir_nil))))
                  (P_ignore
                    (Ir_seq
                      (Ir_ext_call (io format)
                        ((Ir_lit (Lit_string "Hello, ~s!\n"))
                          (Ir_cons
                            (Ir_ext_call (erlang hd)
                              ((Ir_var
                                 ((path ()) (unique_name x_5) (source_name x)))))
                            Ir_nil)))
                      (Ir_apply
                        (Ir_fn_name
                          ((path ()) (unique_name main_4) (source_name main))
                          1)
                        ((Ir_ext_call (erlang tl)
                           ((Ir_var
                              ((path ()) (unique_name x_5) (source_name x))))))))))))))
         (Ir_tuple
           ((Ir_var ((path ()) (unique_name main_4) (source_name main)))))))))

  $ cat Caramel.Hello_world.core
  % Source code generated with Caramel.
  module 'Caramel.Hello_world'
  [
   'main'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.Hello_world') -| [])
  
  'module_info'/1 =
   (fun (Opts) ->
   call 'erlang':'get_module_info'('Caramel.Hello_world', Opts) -| [])
  
  'main'/1 =
   (fun (X) ->
   
     case X of
     <[]> when 'true' ->
       call 'io':'format'(
         #{
           #<126>(8,1,'integer',['unsigned'|['big']]),
           #<115>(8,1,'integer',['unsigned'|['big']]),
           #<10>(8,1,'integer',['unsigned'|['big']])
         }#,
         [
          #{
            #<98>(8,1,'integer',['unsigned'|['big']]),
            #<121>(8,1,'integer',['unsigned'|['big']]),
            #<101>(8,1,'integer',['unsigned'|['big']]),
            #<33>(8,1,'integer',['unsigned'|['big']])
          }#|[] ])
     <_> when 'true' ->
       do
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
             #<115>(8,1,'integer',['unsigned'|['big']]),
             #<33>(8,1,'integer',['unsigned'|['big']]),
             #<10>(8,1,'integer',['unsigned'|['big']])
           }#, [ call 'erlang':'hd'(X)|[] ])
          apply 'main'/1(call 'erlang':'tl'(X))
     end -| [])
  end
  

  $ erlc Caramel.Hello_world.core

  $ escript Caramel.Hello_world.beam Joe Mike Robert
  Hello, Joe!
  Hello, Mike!
  Hello, Robert!
  bye!
