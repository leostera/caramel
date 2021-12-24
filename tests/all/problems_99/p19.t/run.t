19. Rotate a list N places to the left.

  $ caramel compile --debug p19.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p19.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p19.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p19.ml.parsetree
  caramel: [DEBUG] OK
  File "p19.ml", line 34, characters 9-62:
  34 | let main (n :: xs) = format "~p\n" [ rotate xs (parse_int n) ]
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  caramel: [DEBUG] Writing p19.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p19.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p19.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P19.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P19.core

  $ escript Caramel.P19.beam 3 a b c d e f g h
  ["d","e","f","g","h","a","b","c"]
