66. Layout a binary tree (3). (hard)

  $ caramel compile --debug p66.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p66.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p66.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p66.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p66.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p66.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p66.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p66.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p66.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P66.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P66.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P66.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P66.core

  $ escript Caramel.P66.beam
  {node,{<<"n">>,5,1},
        {node,{<<"k">>,3,2},
              {node,{<<"c">>,2,3},
                    {node,{<<"a">>,1,4},empty,empty},
                    {node,{<<"e">>,3,4},
                          {node,{<<"d">>,2,5},empty,empty},
                          {node,{<<"g">>,4,5},empty,empty}}},
              {node,{<<"m">>,4,3},empty,empty}},
        {node,{<<"u">>,7,2},
              {node,{<<"p">>,6,3},empty,{node,{<<"q">>,7,4},empty,empty}},
              empty}}
