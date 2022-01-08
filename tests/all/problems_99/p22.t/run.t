22. Create a list containing all integers within a given range. (easy)

  $ caramel compile --debug p22.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p22.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p22.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p22.ml.parsetree
  caramel: [DEBUG] OK
  File "p22.ml", line 23, characters 9-76:
  23 | let main (a :: b :: _) = format "~p\n" [ range (parse_int a) (parse_int b) ]
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  _::[]
  caramel: [DEBUG] Writing p22.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p22.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p22.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P22.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P22.core

  $ escript Caramel.P22.beam 4 9 
  [4,5,6,7,8,9]
