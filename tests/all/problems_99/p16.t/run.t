16. Drop every N-th element from a list

  $ caramel compile --debug p16.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p16.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p16.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p16.ml.parsetree
  caramel: [DEBUG] OK
  File "p16.ml", line 17, characters 9-60:
  17 | let main (n :: xs) = format "~p\n" [ drop (parse_int n) xs ]
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  caramel: [DEBUG] Writing p16.ml.lambda
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
  caramel: [DEBUG] Writing p16.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p16.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P16.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P16.core

  $ escript Caramel.P16.beam 7 a b c d e f g h i
  ["a","b","c","d","e","f","h","i"]
