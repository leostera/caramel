71. Determine the internal path length of a tree. (easy)

  $ caramel compile --debug p71.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p71.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p71.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p71.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p71.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p71.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p71.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p71.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p71.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p71.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P71.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P71.Binary.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P71.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P71.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P71.core

  $ escript Caramel.P71.beam
  [1,9]
