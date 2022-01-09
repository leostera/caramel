49. Gray code. (medium)

  $ caramel compile --debug p49.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p49.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p49.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p49.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p49.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p49.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p49.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p49.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p49.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P49.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P49.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P49.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P49.core

  $ escript Caramel.P49.beam
  [[<<"0">>,<<"1">>],
   [<<"00">>,<<"01">>,<<"11">>,<<"10">>],
   [<<"000">>,<<"001">>,<<"011">>,<<"010">>,<<"110">>,<<"111">>,<<"101">>,
    <<"100">>]]

