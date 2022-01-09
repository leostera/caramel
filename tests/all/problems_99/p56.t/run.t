56. Symmetric binary trees. (medium)

  $ caramel compile --debug p56.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p56.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p56.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p56.ml.parsetree
  caramel: [DEBUG] OK
  File "p56.ml", lines 106-107, characters 2-52:
  106 | ..let (x :: y :: _) = cbal_tree 4 in
  107 |   format "~p\n" [ [is_symmetric x; is_symmetric y] ]
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  _::[]
  caramel: [DEBUG] Writing p56.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p56.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p56.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p56.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p56.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P56.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P56.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P56.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P56.core

  $ escript Caramel.P56.beam
  [false,false]
