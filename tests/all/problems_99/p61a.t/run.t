61A. Collect the leaves of a binary tree in a list. (easy)

  $ caramel compile --debug p61A.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p61A.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p61A.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p61A.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p61A.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p61A.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p61A.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p61A.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p61A.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P61A.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P61A.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P61A.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P61A.core

  $ escript Caramel.P61A.beam
  [[],[<<"d">>,<<"e">>,<<"g">>]]
