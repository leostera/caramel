59. Construct height-balanced binary trees. (medium)

  $ caramel compile --debug p59.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p59.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p59.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p59.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p59.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] construct Node
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
  caramel: [DEBUG] Writing p59.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p59.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p59.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p59.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P59.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P59.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P59.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P59.core

  $ escript Caramel.P59.beam
  [{node,<<"x">>,
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}},
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}},
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty}},
   {node,<<"x">>,
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}},
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty},
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty},
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty},
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}},
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}},
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}},
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}},
         {node,<<"x">>,empty,empty}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty},
         {node,<<"x">>,empty,empty}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}},
         {node,<<"x">>,empty,empty}},
   {node,<<"x">>,
         {node,<<"x">>,empty,empty},
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,empty,empty},
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty}},
   {node,<<"x">>,
         {node,<<"x">>,empty,empty},
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}}}]
