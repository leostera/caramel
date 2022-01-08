17. Split a list into two parts; the length of the first part is given.

  $ caramel compile --debug p17.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p17.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p17.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p17.ml.parsetree
  caramel: [DEBUG] OK
  File "p17.ml", line 19, characters 9-61:
  19 | let main (n :: xs) = format "~p\n" [ split (parse_int n) xs ]
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  caramel: [DEBUG] Writing p17.ml.lambda
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
  caramel: [DEBUG] Writing p17.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p17.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P17.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P17.core

  $ escript Caramel.P17.beam 2 a b c d e f g
  {["a","b"],["c","d","e","f","g"]}
