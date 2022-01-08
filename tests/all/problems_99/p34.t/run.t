34. Calculate Eulers totient function φ(m). (medium)

  $ caramel compile --debug p34.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p34.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p34.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p34.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p34.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p34.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p34.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p34.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p34.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P34.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P34.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P34.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P34.core

  $ escript Caramel.P34.beam
  [{<<"phi 10">>,4},{<<"phi 13">>,12}]
