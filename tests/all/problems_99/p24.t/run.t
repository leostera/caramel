24. Lotto: Draw N different random numbers from the set 1..M. (easy)

  $ caramel compile --debug p24.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p24.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p24.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p24.ml.parsetree
  caramel: [DEBUG] OK
  File "p24.ml", lines 42-43, characters 4-46:
  42 | ....let (Some (picked, rest)) = extract_rand list len in
  43 |     aux (n - 1) (picked :: acc) rest (len - 1)
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  None
  File "p24.ml", lines 55-57, characters 9-62:
  55 | .........(n0 :: n1 :: _) =
  56 |   Random.seed `default 0;
  57 |   format "~p\n" [ lotto_select (parse_int n0) (parse_int n1) ]
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  _::[]
  caramel: [DEBUG] Writing p24.ml.lambda
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
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p24.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p24.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p24.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P24.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P24.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P24.core

  $ escript Caramel.P24.beam 6 49
  [26,18,35,5,30,38]
