5. Reverse a list

  $ caramel compile --debug p5.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p5.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p5.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p5.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p5.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p5.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p5.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P5.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P5.core

  $ escript Caramel.P5.beam tuli lea lola otto
  ["otto","lola","lea","tuli"]
