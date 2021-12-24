33. Determine whether two positive integer numbers are coprime. (easy)

  $ caramel compile --debug p33.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p33.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p33.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p33.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p33.ml.lambda
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
  caramel: [DEBUG] Writing p33.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p33.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p33.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p33.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P33.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P33.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P33.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P33.core

  $ escript Caramel.P33.beam
  [{<<"coprime 13 27">>,true},{<<"coprime 20536 7826">>,false}]
