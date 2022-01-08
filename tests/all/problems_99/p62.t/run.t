62. Collect the internal nodes of a binary tree in a list. (easy)

  $ caramel compile --debug p62.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p62.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p62.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p62.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p62.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p62.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p62.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p62.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p62.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P62.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P62.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P62.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P62.core

  $ escript Caramel.P62.beam
  [[],[<<"b">>,<<"a">>,<<"c">>,<<"f">>]]
