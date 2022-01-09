67. A string representation of binary trees. (medium)

  $ caramel compile --debug p67.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p67.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p67.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p67.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p67.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p67.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p67.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p67.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p67.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p67.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P67.Binary.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P67.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P67.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P67.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P67.core

  $ escript Caramel.P67.beam
  <<"a(b(d,e),c(,f(g,)))">>
