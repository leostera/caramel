6. Find out whether a list is a palindrome.

  $ caramel compile --debug p6.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p6.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p6.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p6.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p6.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p6.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p6.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P6.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P6.core

  $ escript Caramel.P6.beam 2 1 1 2
  true

  $ escript Caramel.P6.beam 3 4 8 5
  false
