70C. Count the nodes of a multiway tree. (easy)

  $ caramel compile --debug p70c.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p70c.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p70c.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p70c.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p70c.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p70c.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p70c.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p70c.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p70c.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p70c.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P70c.Binary.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P70c.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P70c.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P70c.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P70c.core

  $ escript Caramel.P70c.beam
  2
