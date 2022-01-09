15. Replicate the elements of a list a given number of times.

  $ caramel compile --debug p15.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p15.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p15.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p15.ml.parsetree
  caramel: [DEBUG] OK
  File "p15.ml", line 17, characters 9-65:
  17 | let main (n :: xs) = format "~p\n" [ replicate (parse_int n) xs ]
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  caramel: [DEBUG] Writing p15.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p15.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p15.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P15.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P15.core

  $ escript Caramel.P15.beam 3 a b c
  ["a","a","a","b","b","b","c","c","c"]
