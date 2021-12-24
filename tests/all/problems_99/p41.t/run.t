41. A list of Goldbach compositions. (medium)

  $ caramel compile --debug p41.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p41.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p41.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p41.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p41.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p41.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p41.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p41.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p41.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P41.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P41.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P41.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P41.core

  $ escript Caramel.P41.beam
  [{<<"goldbach_list 9 20">>,
    [{10,{3,7}},{12,{5,7}},{14,{3,11}},{16,{3,13}},{18,{5,13}},{20,{3,17}}]},
   {<<"goldbach_limit 1 2000 50">>,
    [{992,{73,919}},{1382,{61,1321}},{1856,{67,1789}},{1928,{61,1867}}]}]
