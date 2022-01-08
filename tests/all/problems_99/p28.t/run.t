28. Sorting a list of lists according to length of sublists. (medium)

  $ caramel compile --debug p28.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p28.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p28.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p28.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p28.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p28.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p28.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p28.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p28.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P28.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P28.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P28.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P28.core

  $ escript Caramel.P28.beam
  [[<<"i">>,<<"j">>,<<"k">>,<<"l">>],
   [<<"f">>,<<"g">>,<<"h">>],
   [<<"a">>,<<"b">>,<<"c">>],
   [<<"m">>,<<"n">>],
   [<<"d">>,<<"e">>],
   [<<"d">>,<<"e">>],
   [<<"o">>]]
