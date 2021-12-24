35. Determine the prime factors of a given positive integer. (medium)

  $ caramel compile --debug p35.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p35.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p35.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p35.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p35.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p35.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p35.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p35.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p35.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P35.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P35.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P35.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P35.core

  $ escript Caramel.P35.beam
  {<<"factors 315">>,[3,3,5,7]}
