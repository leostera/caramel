Small TCP echo server.

  $ caramel compile --debug echo_server.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (echo_server.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file echo_server.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing echo_server.ml.parsetree
  caramel: [DEBUG] OK
  File "echo_server.ml", lines 20-26, characters 2-31:
  20 | ..let (Ok conn) = accept socket in
  21 |   match recv conn 0 with
  22 |   | Ok data ->
  23 |       dbg "data: ~p~n" [ data ];
  24 |       send conn data;
  25 |       loop socket
  26 |   | Error `closed -> close conn
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  Error _
  File "echo_server.ml", lines 30-34, characters 2-13:
  30 | ..let (Ok socket) =
  31 |     listen port [ `binary; `active false; `packet `line; `reuseaddr true ]
  32 |   in
  33 |   dbg "listening on ~p~n" [ port ];
  34 |   loop socket
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  Error _
  caramel: [DEBUG] Writing echo_server.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access Ok/1
  caramel: [DEBUG] constructor field access Ok/1
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access Ok/1
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing echo_server.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing echo_server.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Echo_server.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ diff Caramel.Echo_server.core Caramel.Echo_server.core.expected
  2c2
  < module 'Caramel.Echo_server'
  ---
  > module 'Echo_server'
  5,7c5
  <  'loop'/1,
  <  'module_info'/0,
  <  'module_info'/1
  ---
  >  'loop'/1
  11,17d8
  < 'module_info'/0 =
  <  (fun () -> call 'erlang':'get_module_info'('Caramel.Echo_server') -| [])
  < 
  < 'module_info'/1 =
  <  (fun (Opts) ->
  <  call 'erlang':'get_module_info'('Caramel.Echo_server', Opts) -| [])
  < 
  21,26c12,17
  <    let <_match__37> = call 'gen_tcp':'listen'(
  <                         Port,
  <                         [ 'binary'|
  <                          [ {'active', 'false'}|
  <                           [ {'packet', 'line'}|[ {'reuseaddr', 'true'}|[] ] ]
  <                          ] ])
  ---
  >    let <_match_> = call 'gen_tcp':'listen'(
  >                      Port,
  >                      [ 'binary'|
  >                       [ {'active', 'false'}|
  >                        [ {'packet', 'line'}|[ {'reuseaddr', 'true'}|[] ] ] ]
  >                      ])
  28c19
  <    case _match__37 of
  ---
  >    case _match_ of
  51c42
  <         apply 'loop'/1(call 'erlang':'element'(2, _match__37))
  ---
  >         apply 'loop'/1(call 'erlang':'element'(2, _match_))
  76,77c67,68
  <  let <_match__36> = call 'gen_tcp':'accept'(Socket) in
  <    case _match__36 of
  ---
  >  let <_match_> = call 'gen_tcp':'accept'(Socket) in
  >    case _match_ of
  79,81c70,72
  <      let <Conn> = call 'erlang':'element'(2, _match__36) in
  <        let <_match__34> = call 'gen_tcp':'recv'(Conn, 0) in
  <        case _match__34 of
  ---
  >      let <Conn> = call 'erlang':'element'(2, _match_) in
  >        let <_match_> = call 'gen_tcp':'recv'(Conn, 0) in
  >        case _match_ of
  83c74
  <          let <Data> = call 'erlang':'element'(2, _match__34) in
  ---
  >          let <Data> = call 'erlang':'element'(2, _match_) in
  [1]

  $ cat echo_server.ml.lambda
  (letrec
    (loop/14
       (function socket/15
         (let (*match*/36 = (gen_tcp:accept socket/15))
           (switch* *match*/36
            case tag 0:
             (let
               (conn/16 =a (field 0 *match*/36)
                *match*/34 = (gen_tcp:recv conn/16 0))
               (switch* *match*/34
                case tag 0:
                 (let (data/17 =a (field 0 *match*/34))
                   (seq (io:format "data: ~p~n" (makeblock 0 data/17 0))
                     (gen_tcp:send conn/16 data/17) (apply loop/14 socket/15)))
                case tag 1: (gen_tcp:close conn/16)))
            case tag 1:
             (raise
               (makeblock 0 (global Match_failure/18!)
                 [0: "echo_server.ml" 20 2]))))))
    (let
      (main/18 =
         (function param/22
           (let
             (port/20 =[int] 2112
              *match*/37 =
                (gen_tcp:listen port/20
                  [0:
                   -317998079
                   [0:
                    [0: 373703110 0]
                    [0: [0: -476555384 -944564236] [0: [0: -459010139 1] 0]]]]))
             (switch* *match*/37
              case tag 0:
               (seq
                 (io:format "listening on ~p~n"
                   (makeblock 0 (int,*) port/20 0))
                 (apply loop/14 (field 0 *match*/37)))
              case tag 1:
               (raise
                 (makeblock 0 (global Match_failure/18!)
                   [0: "echo_server.ml" 30 2]))))))
      (makeblock 0 loop/14 main/18)))

  $ cat echo_server.ml.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Echo_server)
         (source_name Caramel.Echo_server))
       (Ir_letrec
         ((Exported ((path ()) (unique_name loop_14) (source_name loop))
            (Ir_fun (((path ()) (unique_name socket_15) (source_name socket)))
              (Ir_let Private
                ((path ()) (unique_name _match__36) (source_name _match__36))
                (Ir_ext_call (gen_tcp accept)
                  ((Ir_var
                     ((path ()) (unique_name socket_15) (source_name socket)))))
                (Ir_case
                  (Ir_var
                    ((path ()) (unique_name _match__36)
                      (source_name _match__36)))
                  (((P_tuple ((P_lit (Lit_atom ok)) P_ignore))
                     (Ir_let Private
                       ((path ()) (unique_name conn_16) (source_name conn))
                       (Ir_ext_call (erlang element)
                         ((Ir_lit (Lit_int 2))
                           (Ir_var
                             ((path ()) (unique_name _match__36)
                               (source_name _match__36)))))
                       (Ir_let Private
                         ((path ()) (unique_name _match__34)
                           (source_name _match__34))
                         (Ir_ext_call (gen_tcp recv)
                           ((Ir_var
                              ((path ()) (unique_name conn_16)
                                (source_name conn)))
                             (Ir_lit (Lit_int 0))))
                         (Ir_case
                           (Ir_var
                             ((path ()) (unique_name _match__34)
                               (source_name _match__34)))
                           (((P_tuple ((P_lit (Lit_atom ok)) P_ignore))
                              (Ir_let Private
                                ((path ()) (unique_name data_17)
                                  (source_name data))
                                (Ir_ext_call (erlang element)
                                  ((Ir_lit (Lit_int 2))
                                    (Ir_var
                                      ((path ()) (unique_name _match__34)
                                        (source_name _match__34)))))
                                (Ir_seq
                                  (Ir_ext_call (io format)
                                    ((Ir_lit (Lit_string "data: ~p~n"))
                                      (Ir_cons
                                        (Ir_var
                                          ((path ()) (unique_name data_17)
                                            (source_name data)))
                                        Ir_nil)))
                                  (Ir_seq
                                    (Ir_ext_call (gen_tcp send)
                                      ((Ir_var
                                         ((path ()) (unique_name conn_16)
                                           (source_name conn)))
                                        (Ir_var
                                          ((path ()) (unique_name data_17)
                                            (source_name data)))))
                                    (Ir_apply
                                      (Ir_fn_name
                                        ((path ()) (unique_name loop_14)
                                          (source_name loop))
                                        1)
                                      ((Ir_var
                                         ((path ()) (unique_name socket_15)
                                           (source_name socket)))))))))
                             ((P_tuple ((P_lit (Lit_atom error)) P_ignore))
                               (Ir_ext_call (gen_tcp close)
                                 ((Ir_var
                                    ((path ()) (unique_name conn_16)
                                      (source_name conn)))))))))))
                    ((P_tuple ((P_lit (Lit_atom error)) P_ignore))
                      (Ir_ext_call (erlang throw)
                        ((Ir_tuple
                           ((Ir_tuple
                              ((Ir_lit (Lit_atom EXIT))
                                (Ir_lit (Lit_atom badmatch))))
                             (Ir_tuple
                               ((Ir_lit (Lit_string echo_server.ml))
                                 (Ir_lit (Lit_int 20)) (Ir_lit (Lit_int 2)))))))))))))))
         (Ir_let Exported ((path ()) (unique_name main_18) (source_name main))
           (Ir_fun (((path ()) (unique_name param_22) (source_name param)))
             (Ir_let Private
               ((path ()) (unique_name port_20) (source_name port))
               (Ir_lit (Lit_int 2112))
               (Ir_let Private
                 ((path ()) (unique_name _match__37) (source_name _match__37))
                 (Ir_ext_call (gen_tcp listen)
                   ((Ir_var
                      ((path ()) (unique_name port_20) (source_name port)))
                     (Ir_cons (Ir_lit (Lit_atom binary))
                       (Ir_cons
                         (Ir_tuple
                           ((Ir_lit (Lit_atom active))
                             (Ir_lit (Lit_atom false))))
                         (Ir_cons
                           (Ir_tuple
                             ((Ir_lit (Lit_atom packet))
                               (Ir_lit (Lit_atom line))))
                           (Ir_cons
                             (Ir_tuple
                               ((Ir_lit (Lit_atom reuseaddr))
                                 (Ir_lit (Lit_atom true))))
                             Ir_nil))))))
                 (Ir_case
                   (Ir_var
                     ((path ()) (unique_name _match__37)
                       (source_name _match__37)))
                   (((P_tuple ((P_lit (Lit_atom ok)) P_ignore))
                      (Ir_seq
                        (Ir_ext_call (io format)
                          ((Ir_lit (Lit_string "listening on ~p~n"))
                            (Ir_cons
                              (Ir_var
                                ((path ()) (unique_name port_20)
                                  (source_name port)))
                              Ir_nil)))
                        (Ir_apply
                          (Ir_fn_name
                            ((path ()) (unique_name loop_14)
                              (source_name loop))
                            1)
                          ((Ir_ext_call (erlang element)
                             ((Ir_lit (Lit_int 2))
                               (Ir_var
                                 ((path ()) (unique_name _match__37)
                                   (source_name _match__37)))))))))
                     ((P_tuple ((P_lit (Lit_atom error)) P_ignore))
                       (Ir_ext_call (erlang throw)
                         ((Ir_tuple
                            ((Ir_tuple
                               ((Ir_lit (Lit_atom EXIT))
                                 (Ir_lit (Lit_atom badmatch))))
                              (Ir_tuple
                                ((Ir_lit (Lit_string echo_server.ml))
                                  (Ir_lit (Lit_int 30)) (Ir_lit (Lit_int 2))))))))))))))
           (Ir_tuple
             ((Ir_var ((path ()) (unique_name loop_14) (source_name loop)))
               (Ir_var ((path ()) (unique_name main_18) (source_name main))))))))))

  $ cat Caramel.Echo_server.core
  % Source code generated with Caramel.
  module 'Caramel.Echo_server'
  [
   'main'/1,
   'loop'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.Echo_server') -| [])
  
  'module_info'/1 =
   (fun (Opts) ->
   call 'erlang':'get_module_info'('Caramel.Echo_server', Opts) -| [])
  
  'main'/1 =
   (fun (Param) ->
   let <Port> = 2112 in
     let <_match__37> = call 'gen_tcp':'listen'(
                          Port,
                          [ 'binary'|
                           [ {'active', 'false'}|
                            [ {'packet', 'line'}|[ {'reuseaddr', 'true'}|[] ] ]
                           ] ])
      in
     case _match__37 of
     <{'ok', _}> when 'true' ->
       do
         call 'io':'format'(
           #{
             #<108>(8,1,'integer',['unsigned'|['big']]),
             #<105>(8,1,'integer',['unsigned'|['big']]),
             #<115>(8,1,'integer',['unsigned'|['big']]),
             #<116>(8,1,'integer',['unsigned'|['big']]),
             #<101>(8,1,'integer',['unsigned'|['big']]),
             #<110>(8,1,'integer',['unsigned'|['big']]),
             #<105>(8,1,'integer',['unsigned'|['big']]),
             #<110>(8,1,'integer',['unsigned'|['big']]),
             #<103>(8,1,'integer',['unsigned'|['big']]),
             #<32>(8,1,'integer',['unsigned'|['big']]),
             #<111>(8,1,'integer',['unsigned'|['big']]),
             #<110>(8,1,'integer',['unsigned'|['big']]),
             #<32>(8,1,'integer',['unsigned'|['big']]),
             #<126>(8,1,'integer',['unsigned'|['big']]),
             #<112>(8,1,'integer',['unsigned'|['big']]),
             #<126>(8,1,'integer',['unsigned'|['big']]),
             #<110>(8,1,'integer',['unsigned'|['big']])
           }#, [ Port|[] ])
          apply 'loop'/1(call 'erlang':'element'(2, _match__37))
     <{'error', _}> when 'true' ->
       call 'erlang':'throw'(
         {{'EXIT', 'badmatch'},
          {
           #{
             #<101>(8,1,'integer',['unsigned'|['big']]),
             #<99>(8,1,'integer',['unsigned'|['big']]),
             #<104>(8,1,'integer',['unsigned'|['big']]),
             #<111>(8,1,'integer',['unsigned'|['big']]),
             #<95>(8,1,'integer',['unsigned'|['big']]),
             #<115>(8,1,'integer',['unsigned'|['big']]),
             #<101>(8,1,'integer',['unsigned'|['big']]),
             #<114>(8,1,'integer',['unsigned'|['big']]),
             #<118>(8,1,'integer',['unsigned'|['big']]),
             #<101>(8,1,'integer',['unsigned'|['big']]),
             #<114>(8,1,'integer',['unsigned'|['big']]),
             #<46>(8,1,'integer',['unsigned'|['big']]),
             #<109>(8,1,'integer',['unsigned'|['big']]),
             #<108>(8,1,'integer',['unsigned'|['big']])
           }#, 30, 2}})
     end -| [])
  
  'loop'/1 =
   (fun (Socket) ->
   let <_match__36> = call 'gen_tcp':'accept'(Socket) in
     case _match__36 of
     <{'ok', _}> when 'true' ->
       let <Conn> = call 'erlang':'element'(2, _match__36) in
         let <_match__34> = call 'gen_tcp':'recv'(Conn, 0) in
         case _match__34 of
         <{'ok', _}> when 'true' ->
           let <Data> = call 'erlang':'element'(2, _match__34) in
             do
               call 'io':'format'(
                 #{
                   #<100>(8,1,'integer',['unsigned'|['big']]),
                   #<97>(8,1,'integer',['unsigned'|['big']]),
                   #<116>(8,1,'integer',['unsigned'|['big']]),
                   #<97>(8,1,'integer',['unsigned'|['big']]),
                   #<58>(8,1,'integer',['unsigned'|['big']]),
                   #<32>(8,1,'integer',['unsigned'|['big']]),
                   #<126>(8,1,'integer',['unsigned'|['big']]),
                   #<112>(8,1,'integer',['unsigned'|['big']]),
                   #<126>(8,1,'integer',['unsigned'|['big']]),
                   #<110>(8,1,'integer',['unsigned'|['big']])
                 }#, [ Data|[] ])
                do call 'gen_tcp':'send'(Conn, Data) apply 'loop'/1(Socket)
         <{'error', _}> when 'true' -> call 'gen_tcp':'close'(Conn)
         end
     <{'error', _}> when 'true' ->
       call 'erlang':'throw'(
         {{'EXIT', 'badmatch'},
          {
           #{
             #<101>(8,1,'integer',['unsigned'|['big']]),
             #<99>(8,1,'integer',['unsigned'|['big']]),
             #<104>(8,1,'integer',['unsigned'|['big']]),
             #<111>(8,1,'integer',['unsigned'|['big']]),
             #<95>(8,1,'integer',['unsigned'|['big']]),
             #<115>(8,1,'integer',['unsigned'|['big']]),
             #<101>(8,1,'integer',['unsigned'|['big']]),
             #<114>(8,1,'integer',['unsigned'|['big']]),
             #<118>(8,1,'integer',['unsigned'|['big']]),
             #<101>(8,1,'integer',['unsigned'|['big']]),
             #<114>(8,1,'integer',['unsigned'|['big']]),
             #<46>(8,1,'integer',['unsigned'|['big']]),
             #<109>(8,1,'integer',['unsigned'|['big']]),
             #<108>(8,1,'integer',['unsigned'|['big']])
           }#, 20, 2}})
     end -| [])
  end
  

  $ erlc Caramel.Echo_server.core
