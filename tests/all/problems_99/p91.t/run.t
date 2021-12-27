91. Eight queens problem. (medium)

  $ caramel compile --debug p91.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p91.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p91.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p91.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p91.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p91.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p91.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p91.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p91.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p91.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P91.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P91.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P91.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P91.Binary.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P91.core

  $ escript Caramel.P91.beam
  [[3,1,4,2],[2,4,1,3]]
