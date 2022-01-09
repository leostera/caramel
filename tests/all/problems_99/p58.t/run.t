58. Generate-and-test paradigm. (medium)

  $ caramel compile --debug p58.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p58.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p58.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p58.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p58.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p58.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p58.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p58.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p58.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P58.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P58.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P58.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P58.core

  $ escript Caramel.P58.beam
  [{node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty},
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}},
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty}}]
