4. Find the number of elements of a list.

  $ caramel compile --debug p4.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p4.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p4.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p4.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p4.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p4.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p4.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P4.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P4.core

  $ escript Caramel.P4.beam 1 a b c 2 3 4
  7
