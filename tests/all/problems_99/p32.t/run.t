32. Determine the greatest common divisor of two positive integer numbers. (medium)

  $ caramel compile --debug p32.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p32.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p32.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p32.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p32.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p32.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p32.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p32.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p32.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P32.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P32.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P32.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P32.core

  $ escript Caramel.P32.beam
  [{<<"gcd 13 27">>,1},{<<"gcd 20536 7826">>,2}]
