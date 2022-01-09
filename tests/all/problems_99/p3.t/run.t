3. Find the K-th element of a list.

  $ caramel compile --debug p3.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p3.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p3.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p3.ml.parsetree
  caramel: [DEBUG] OK
  File "p3.ml", line 13, characters 13-44:
  13 | let rec main (k :: ls) = at (parse_int k) ls
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  caramel: [DEBUG] Writing p3.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p3.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p3.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P3.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P3.core

  $ escript Caramel.P3.beam 2 a b c d e
  "b"
