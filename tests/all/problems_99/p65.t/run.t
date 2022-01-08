65. Layout a binary tree (2). (medium)

  $ caramel compile --debug p65.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p65.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p65.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p65.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p65.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
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
  caramel: [DEBUG] Writing p65.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p65.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p65.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p65.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P65.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P65.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P65.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P65.core

  $ escript Caramel.P65.beam
  {node,{<<"n">>,15,1},
        {node,{<<"k">>,7,2},
              {node,{<<"c">>,3,3},
                    {node,{<<"a">>,1,4},empty,empty},
                    {node,{<<"e">>,5,4},
                          {node,{<<"d">>,4,5},empty,empty},
                          {node,{<<"g">>,6,5},empty,empty}}},
              {node,{<<"m">>,11,3},empty,empty}},
        {node,{<<"u">>,23,2},
              {node,{<<"p">>,19,3},empty,{node,{<<"q">>,21,4},empty,empty}},
              empty}}
