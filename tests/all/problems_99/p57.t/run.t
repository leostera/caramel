57. Binary search trees (dictionaries). (medium)

  $ caramel compile --debug p57.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p57.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p57.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p57.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p57.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] tuple
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
  caramel: [DEBUG] Writing p57.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p57.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p57.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p57.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P57.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P57.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P57.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P57.core

  $ escript Caramel.P57.beam
  {node,3,
        {node,2,{node,1,empty,empty},empty},
        {node,5,empty,{node,7,empty,empty}}}
