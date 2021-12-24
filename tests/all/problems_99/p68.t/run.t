68. Preorder and inorder sequences of binary trees. (medium)

  $ caramel compile --debug p68.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p68.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p68.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p68.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p68.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
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
  caramel: [DEBUG] Writing p68.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p68.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p68.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p68.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p68.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P68.Binary.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P68.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P68.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P68.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P68.core

  $ escript Caramel.P68.beam
  [[1,2],[1,2]]
