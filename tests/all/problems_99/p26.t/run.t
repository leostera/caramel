26. Generate the combinations of K distinct objects chosen from the N elements of a list. (medium)

  $ caramel compile --debug p26.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p26.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p26.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p26.ml.parsetree
  caramel: [DEBUG] OK
  File "p26.ml", line 49, characters 9-63:
  49 | let main (n :: xs) = format "~p\n" [ extract (parse_int n) xs ]
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  caramel: [DEBUG] Writing p26.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p26.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p26.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p26.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p26.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P26.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P26.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P26.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P26.core

  $ escript Caramel.P26.beam 2 a b c d
  [["a","b"],["a","c"],["a","d"],["b","c"],["b","d"],["c","d"]]
