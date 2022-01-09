1. Write a function last : 'a list -> 'a option that returns the last element
of a list.

  $ caramel compile --debug p1.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p1.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p1.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p1.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p1.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p1.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p1.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P1.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P1.core

  $ escript Caramel.P1.beam 1 2 3 4
  "4"
