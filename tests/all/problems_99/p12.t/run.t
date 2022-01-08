12. Decode a run-length encoded list.

  $ caramel compile --debug p12.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p12.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p12.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p12.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p12.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access One/1
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] constructor field access Many/2
  caramel: [DEBUG] constructor field access Many/2
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p12.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p12.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P12.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P12.core

  $ escript Caramel.P12.beam
  [<<"a">>,<<"a">>,<<"a">>,<<"a">>,<<"b">>,<<"c">>,<<"c">>,<<"a">>,<<"a">>,
   <<"d">>,<<"e">>,<<"e">>,<<"e">>,<<"e">>]
