21. Insert an element at a given position into a list

  $ caramel compile --debug p21.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p21.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p21.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p21.ml.parsetree
  caramel: [DEBUG] OK
  File "p21.ml", line 23, characters 9-74:
  23 | let main (el :: n :: xs) = format "~p\n" [ insert_at el (parse_int n) xs ]
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  _::[]
  caramel: [DEBUG] Writing p21.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p21.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p21.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P21.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P21.core

  $ escript Caramel.P21.beam alfa 3 a b c d 
  ["a","b","c","alfa","d"]

