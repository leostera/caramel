72. Construct the bottom-up order sequence of the tree nodes. (easy)

  $ caramel compile --debug p72.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p72.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p72.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p72.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p72.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
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
