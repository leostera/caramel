2. Find the last but one (last and penultimate) elements of a list.

  $ caramel compile --debug p2.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p2.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p2.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p2.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p2.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p2.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p2.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P2.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P2.core

  $ escript Caramel.P2.beam 1 2 3 4
  {"3","4"}
