20. Remove the K-th element from a list.

  $ caramel compile --debug p20.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p20.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p20.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p20.ml.parsetree
  caramel: [DEBUG] OK
  File "p20.ml", line 23, characters 9-65:
  23 | let main (n :: xs) = format "~p\n" [ remove_at (parse_int n) xs ]
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  caramel: [DEBUG] Writing p20.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p20.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p20.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P20.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P20.core

  $ escript Caramel.P20.beam 2 a b c d
  ["a","b","d"]
