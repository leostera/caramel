18. Extract a slice from a list.

  $ caramel compile --debug p18.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p18.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p18.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p18.ml.parsetree
  caramel: [DEBUG] OK
  File "p18.ml", line 24, characters 9-84:
  24 | let main (i0 :: i1 :: xs) = format "~p\n" [ slice xs (parse_int i0) (parse_int i1) ]
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  _::[]
  caramel: [DEBUG] Writing p18.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p18.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p18.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P18.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P18.core

  $ escript Caramel.P18.beam 2 6 a b c d e f g h i j k l m n
  ["c","d","e","f","g"]
