36. Determine the prime factors of a given positive integer (2). (medium)

  $ caramel compile --debug p36.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p36.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p36.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p36.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p36.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
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
