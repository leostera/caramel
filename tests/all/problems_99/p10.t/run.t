10. Run-length encoding of a list.

  $ caramel compile --debug p10.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p10.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p10.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p10.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p10.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p10.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p10.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P10.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P10.core

  $ escript Caramel.P10.beam
  [{4,<<"a">>},{1,<<"b">>},{2,<<"c">>},{2,<<"a">>},{1,<<"d">>},{4,<<"e">>}]
