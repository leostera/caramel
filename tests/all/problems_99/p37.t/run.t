37. Calculate Eulers totient function φ(m) (improved). (medium)

  $ caramel compile --debug p37.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p37.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p37.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p37.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p37.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p37.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p37.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p37.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p37.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P37.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P37.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P37.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P37.core

  $ escript Caramel.P37.beam
  [{<<"phi_improved 10">>,4},{<<"phi_improved 13">>,12}]
