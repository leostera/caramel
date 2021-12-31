  $ cd src;

  $ caramel compile --debug erlang.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (erlang.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file erlang.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing erlang.ml.parsetree
  caramel: [DEBUG] OK
  File "erlang.ml", lines 94-95, characters 2-29:
  94 | ..external start_link : register -> 'b -> 'c list -> (pid, 'err) result
  95 |     = "supervisor:start_link"
  Warning 61 [unboxable-type-in-prim-decl]: This primitive declaration uses type register, whose representation
  may be either boxed or unboxed. Without an annotation to indicate
  which representation is intended, the boxed representation has been
  selected by default. This default choice may change in future
  versions of the compiler, breaking the primitive implementation.
  You should explicitly annotate the declaration of register
  with [@@boxed] or [@@unboxed], so that its external interface
  remains stable in the future.
  caramel: [DEBUG] Writing erlang.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing erlang.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing erlang.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing erlang.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing erlang.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing erlang.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing erlang.ml.b_4
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Erlang.Io.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Erlang.Io_lib.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Erlang.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Erlang.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Erlang.Supervisor.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ caramel compile --debug elli.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (elli.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file elli.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing elli.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing elli.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing elli.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing elli.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing elli.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Elli.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Elli.Request.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ caramel compile --debug http_method.ml http_status.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (http_method.ml http_status.ml)) (stdlib (./))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file http_method.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing http_method.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing http_method.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] variant Other
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] Writing http_method.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing http_method.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Http_method.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done
  caramel: [DEBUG] Compiling unit: ((source_file http_status.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing http_status.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing http_status.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] variant Code
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] variant Code
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] variant Code
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] Writing http_status.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing http_status.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing http_status.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Http_status.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Http_method.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ caramel compile --debug main_sup.ml main_app.ml main.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (main_sup.ml main_app.ml main.ml)) (stdlib (./))
    (dump_parsetree true) (dump_typedtree true) (dump_ir true) (dump_pass -1)
    (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file main_sup.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing main_sup.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing main_sup.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] construct Ok
  caramel: [DEBUG] construct Callback
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] construct Local
  caramel: [DEBUG] Writing main_sup.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing main_sup.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main_sup.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done
  caramel: [DEBUG] Compiling unit: ((source_file main_app.ml)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing main_app.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing main_app.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing main_app.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing main_app.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing main_app.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main_app.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main_sup.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done
  caramel: [DEBUG] Compiling unit: ((source_file main.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing main.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing main.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] Writing main.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing main.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing main.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing main.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main_app.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main_sup.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc *.core
