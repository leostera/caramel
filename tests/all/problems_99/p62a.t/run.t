62B. Collect the nodes at a given level in a list. (easy)

  $ caramel compile --debug p62a.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p62a.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p62a.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p62a.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p62a.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p62a.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p62a.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p62a.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p62a.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P62a.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P62a.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P62a.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P62a.core

  $ escript Caramel.P62a.beam
  [[<<"b">>,<<"c">>],[]]
