25. Generate a random permutation of the elements of a list. (easy)

  $ caramel compile --debug p25.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p25.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p25.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p25.ml.parsetree
  caramel: [DEBUG] OK
  File "p25.ml", lines 42-43, characters 4-38:
  42 | ....let (Some (picked, rest)) = extract_rand list len in
  43 |     aux (picked :: acc) rest (len - 1)
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  None
  caramel: [DEBUG] Writing p25.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] construct Some
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] constructor field access Some/1
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p25.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p25.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p25.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P25.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P25.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P25.core

  $ escript Caramel.P25.beam a b c d e f 
  ["c","a","f","e","d"]
