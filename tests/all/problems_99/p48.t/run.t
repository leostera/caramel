48. Truth tables for logical expressions. (medium)

  $ caramel compile --debug p48.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p48.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p48.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p48.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p48.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p48.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p48.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p48.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p48.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P48.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P48.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P48.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P48*.core

  $ escript Caramel.P48.beam
  [{[{<<"a">>,true},{<<"b">>,true}],true},
   {[{<<"a">>,true},{<<"b">>,false}],true},
   {[{<<"a">>,false},{<<"b">>,true}],false},
   {[{<<"a">>,false},{<<"b">>,false}],false}]

