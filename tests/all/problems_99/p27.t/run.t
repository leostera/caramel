27. Group the elements of a set into disjoint subsets. (medium)

  $ caramel compile --debug p27.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p27.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p27.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p27.ml.parsetree
  caramel: [DEBUG] OK
  File "p27.ml", lines 65-66, characters 9-57:
  65 | .........(n :: m :: xs) =
  66 |   format "~p\n" [ group xs [ parse_int n; parse_int m ] ]
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  _::[]
  caramel: [DEBUG] Writing p27.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing p27.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p27.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p27.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p27.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P27.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P27.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.P27.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc Caramel.P27.core

  $ escript Caramel.P27.beam 2 1 a b c d
  [[["a","b"],["c"]],
   [["a","c"],["b"]],
   [["b","c"],["a"]],
   [["a","b"],["d"]],
   [["a","c"],["d"]],
   [["b","c"],["d"]],
   [["a","d"],["b"]],
   [["b","d"],["a"]],
   [["a","d"],["c"]],
   [["b","d"],["c"]],
   [["c","d"],["a"]],
   [["c","d"],["b"]]]
