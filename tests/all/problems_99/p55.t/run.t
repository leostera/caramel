55. Construct completely balanced binary trees. (medium)

  $ caramel compile --debug p55.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p55.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p55.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p55.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p55.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p55.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p55.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p55.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p55.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P55.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P55.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P55.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P55.core

  $ escript Caramel.P55.beam
  [{node,<<"x">>,
         {node,<<"x">>,empty,empty},
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty}},
   {node,<<"x">>,
         {node,<<"x">>,empty,empty},
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty},
         {node,<<"x">>,empty,empty}},
   {node,<<"x">>,
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}},
         {node,<<"x">>,empty,empty}}]
