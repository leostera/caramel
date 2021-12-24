11. Modified run-length encoding.

  $ caramel compile --debug p11.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p11.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p11.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p11.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p11.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] construct One
  caramel: [DEBUG] construct Many
  caramel: [DEBUG] Writing p11.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p11.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P11.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P11.core

  $ escript Caramel.P11.beam
  [{many,4,<<"a">>},
   {one,<<"b">>},
   {many,2,<<"c">>},
   {many,2,<<"a">>},
   {one,<<"d">>},
   {many,4,<<"e">>}]
