8. Eliminate consecutive duplicates of list elements.

  $ caramel compile --debug p8.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p8.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p8.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p8.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p8.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p8.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p8.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P8.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P8.core

  $ escript Caramel.P8.beam
  [<<"a">>,<<"b">>,<<"c">>,<<"a">>,<<"d">>,<<"e">>]
