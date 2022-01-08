14. Duplicate the elements of a list.

  $ caramel compile --debug p14.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p14.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p14.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p14.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p14.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p14.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p14.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P14.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P14.core

  $ escript Caramel.P14.beam 2 1 1 2
  ["2","2","1","1","1","1","2","2"]
