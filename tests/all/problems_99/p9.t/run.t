9. Pack consecutive duplicates of list elements into sublists.

  $ caramel compile --debug p9.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p9.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p9.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p9.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p9.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p9.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p9.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P9.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P9.core

  $ escript Caramel.P9.beam
  [[<<"a">>,<<"a">>,<<"a">>,<<"a">>],
   [<<"b">>],
   [<<"c">>,<<"c">>],
   [<<"a">>,<<"a">>],
   [<<"d">>],
   [<<"e">>,<<"e">>,<<"e">>,<<"e">>]]
