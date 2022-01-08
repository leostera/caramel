31. Determine whether a given integer number is prime. (medium)

  $ caramel compile --debug p31.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p31.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p31.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p31.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p31.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p31.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p31.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p31.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p31.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P31.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P31.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P31.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P31.core

  $ escript Caramel.P31.beam
  [false,false,false]
