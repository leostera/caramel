23. Extract a given number of randomly selected elements from a list. (medium)

  $ caramel compile --debug p23.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p23.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p23.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p23.ml.parsetree
  caramel: [DEBUG] OK
  File "p23.ml", lines 42-43, characters 4-46:
  42 | ....let (Some (picked, rest)) = extract_rand list len in
  43 |     aux (n - 1) (picked :: acc) rest (len - 1)
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  None
  File "p23.ml", lines 49-51, characters 9-48:
  49 | .........(n :: xs) =
  50 |   Random.seed `default 0;
  51 |   format "~p\n" [ rand_select xs (parse_int n) ]
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  caramel: [DEBUG] Writing p23.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p23.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p23.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p23.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P23.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P23.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P23.core

  $ escript Caramel.P23.beam 3 a b c d e f g h
  ["f","b","e"]
