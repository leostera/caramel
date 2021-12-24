72. Construct the bottom-up order sequence of the tree nodes. (easy)

  $ caramel compile --debug p72.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p72.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p72.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p72.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p72.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access T/2
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access T/2
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p72.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p72.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p72.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p72.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p72.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P72.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P72.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P72.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P72.Binary.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P72.core

  $ escript Caramel.P72.beam
  [[<<"b">>,<<"a">>],[<<"g">>,<<"f">>,<<"c">>,<<"d">>,<<"e">>,<<"b">>,<<"a">>]]
