40. Goldbachs conjecture. (medium)

  $ caramel compile --debug p40.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p40.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p40.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p40.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p40.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p40.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p40.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p40.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p40.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P40.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P40.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P40.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P40.core

  $ escript Caramel.P40.beam
  {<<"goldbach 28">>,{5,23}}
