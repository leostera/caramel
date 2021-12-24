36. Determine the prime factors of a given positive integer (2). (medium)

  $ caramel compile --debug p36.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p36.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p36.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p36.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p36.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p36.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p36.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p36.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p36.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P36.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P36.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P36.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P36.core

  $ escript Caramel.P36.beam
  {<<"factors 315">>,[{3,2},{5,1},{7,1}]}
