7. Flatten a nested list structure

  $ caramel compile --debug p7.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p7.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p7.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p7.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p7.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p7.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p7.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P7.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P7.core

  $ escript Caramel.P7.beam
  [<<"a">>,<<"b">>,<<"c">>,<<"d">>,<<"e">>]
