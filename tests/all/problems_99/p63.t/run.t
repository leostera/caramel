63. Construct a complete binary tree. (medium)

  $ caramel compile --debug p63.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p63.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p63.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p63.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p63.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p63.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p63.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p63.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p63.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P63.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P63.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P63.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P63.core

  $ escript Caramel.P63.beam
  {node,1,
        {node,2,{node,4,empty,empty},{node,5,empty,empty}},
        {node,3,{node,6,empty,empty},empty}}
