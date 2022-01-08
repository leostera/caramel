46 & 47. Truth tables for logical expressions (2 variables). (medium)

  $ caramel compile --debug p46.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p46.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p46.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p46.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p46.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Var/1
  caramel: [DEBUG] constructor field access Not/1
  caramel: [DEBUG] constructor field access And/2
  caramel: [DEBUG] constructor field access And/2
  caramel: [DEBUG] constructor field access Or/2
  caramel: [DEBUG] constructor field access Or/2
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p46.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p46.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p46.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p46.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P46.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P46.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P46.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P46.core

  $ escript Caramel.P46.beam
  [{true,true,true},{true,false,true},{false,true,false},{false,false,false}]
