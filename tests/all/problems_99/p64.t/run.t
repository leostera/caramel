64. Layout a binary tree (1). (medium)

  $ caramel compile --debug p64.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p64.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p64.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p64.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p64.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p64.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p64.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p64.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p64.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P64.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P64.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P64.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P64.core

  $ escript Caramel.P64.beam
  {node,{<<"n">>,8,1},
        {node,{<<"k">>,6,2},
              {node,{<<"c">>,2,3},
                    {node,{<<"a">>,1,4},empty,empty},
                    {node,{<<"h">>,5,4},
                          {node,{<<"g">>,4,5},
                                {node,{<<"e">>,3,6},empty,empty},
                                empty},
                          empty}},
              {node,{<<"m">>,7,3},empty,empty}},
        {node,{<<"u">>,12,2},
              {node,{<<"p">>,9,3},
                    empty,
                    {node,{<<"s">>,11,4},
                          {node,{<<"q">>,10,5},empty,empty},
                          empty}},
              empty}}
