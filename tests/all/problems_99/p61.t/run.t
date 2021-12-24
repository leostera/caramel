61. Count the leaves of a binary tree. (easy)

  $ caramel compile --debug p61.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p61.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p61.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p61.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p61.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p61.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p61.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p61.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p61.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P61.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P61.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P61.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P61.core

  $ escript Caramel.P61.beam
  [0,3]
