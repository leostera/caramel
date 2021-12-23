# 99 problems in OCaml, one by one.

================================================================================

1. Write a function last : 'a list -> 'a option that returns the last element
of a list.

  $ caramel compile --debug p1.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p1.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p1.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p1.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p1.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p1.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p1.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P1.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P1.core

  $ escript P1.beam 1 2 3 4
  "4"

================================================================================

2. Find the last but one (last and penultimate) elements of a list.

  $ caramel compile --debug p2.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p2.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p2.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p2.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p2.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p2.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p2.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P2.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P2.core

  $ escript P2.beam 1 2 3 4
  {"3","4"}


================================================================================

3. Find the K-th element of a list.

  $ caramel compile --debug p3.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p3.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
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
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p3.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p3.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P3.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P3.core

  $ escript P3.beam 2 a b c d e
  "b"

================================================================================

4. Find the number of elements of a list.

  $ caramel compile --debug p4.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p4.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p4.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p4.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p4.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p4.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p4.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P4.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P4.core

  $ escript P4.beam 1 a b c 2 3 4
  7

================================================================================

5. Reverse a list

  $ caramel compile --debug p5.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p5.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p5.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p5.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p5.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p5.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p5.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P5.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P5.core

  $ escript P5.beam tuli lea lola otto
  ["otto","lola","lea","tuli"]

===============================================================================

6. Find out whether a list is a palindrome.

  $ caramel compile --debug p6.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p6.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p6.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p6.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p6.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p6.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p6.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P6.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P6.core

  $ escript P6.beam 2 1 1 2
  true

  $ escript P6.beam 3 4 8 5
  false

===============================================================================

7. Flatten a nested list structure

  $ caramel compile --debug p7.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p7.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p7.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p7.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p7.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access One/1
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] constructor field access Many/1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p7.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p7.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P7.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P7.core

  $ escript P7.beam
  [<<"a">>,<<"b">>,<<"c">>,<<"d">>,<<"e">>]

===============================================================================

8. Eliminate consecutive duplicates of list elements.

  $ caramel compile --debug p8.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p8.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p8.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p8.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p8.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p8.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p8.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P8.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P8.core

  $ escript P8.beam
  [<<"a">>,<<"b">>,<<"c">>,<<"a">>,<<"d">>,<<"e">>]

===============================================================================

9. Pack consecutive duplicates of list elements into sublists.

  $ caramel compile --debug p9.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p9.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p9.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p9.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p9.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p9.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p9.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P9.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P9.core

  $ escript P9.beam
  [[<<"a">>,<<"a">>,<<"a">>,<<"a">>],
   [<<"b">>],
   [<<"c">>,<<"c">>],
   [<<"a">>,<<"a">>],
   [<<"d">>],
   [<<"e">>,<<"e">>,<<"e">>,<<"e">>]]

===============================================================================

10. Run-length encoding of a list.

  $ caramel compile --debug p10.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p10.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p10.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p10.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p10.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p10.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p10.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P10.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P10.core

  $ escript P10.beam
  [{4,<<"a">>},{1,<<"b">>},{2,<<"c">>},{2,<<"a">>},{1,<<"d">>},{4,<<"e">>}]

===============================================================================

11. Modified run-length encoding.

  $ caramel compile --debug p11.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p11.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p11.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p11.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p11.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] construct One
  caramel: [DEBUG] construct Many
  caramel: [DEBUG] Writing p11.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p11.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P11.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P11.core

  $ escript P11.beam
  [{many,4,<<"a">>},
   {one,<<"b">>},
   {many,2,<<"c">>},
   {many,2,<<"a">>},
   {one,<<"d">>},
   {many,4,<<"e">>}]

===============================================================================

12. Decode a run-length encoded list.

  $ caramel compile --debug p12.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p12.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p12.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p12.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p12.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access One/1
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] constructor field access Many/2
  caramel: [DEBUG] constructor field access Many/2
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p12.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p12.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P12.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P12.core

  $ escript P12.beam
  [<<"a">>,<<"a">>,<<"a">>,<<"a">>,<<"b">>,<<"c">>,<<"c">>,<<"a">>,<<"a">>,
   <<"d">>,<<"e">>,<<"e">>,<<"e">>,<<"e">>]

===============================================================================

13. Run-length encoding of a list (direct solution)

  $ caramel compile --debug p13.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p13.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p13.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p13.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p13.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] construct One
  caramel: [DEBUG] construct Many
  caramel: [DEBUG] Writing p13.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p13.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P13.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P13.core

  $ escript P13.beam
  [{many,4,<<"a">>},
   {one,<<"b">>},
   {many,2,<<"c">>},
   {many,2,<<"a">>},
   {one,<<"d">>},
   {many,4,<<"e">>}]

===============================================================================

14. Duplicate the elements of a list.

  $ caramel compile --debug p14.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p14.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p14.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p14.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p14.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p14.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p14.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P14.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P14.core

  $ escript P14.beam 2 1 1 2
  ["2","2","1","1","1","1","2","2"]

===============================================================================

15. Replicate the elements of a list a given number of times.

  $ caramel compile --debug p15.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p15.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p15.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p15.ml.parsetree
  caramel: [DEBUG] OK
  File "p15.ml", line 17, characters 9-65:
  17 | let main (n :: xs) = format "~p\n" [ replicate (parse_int n) xs ]
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  caramel: [DEBUG] Writing p15.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p15.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p15.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P15.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P15.core

  $ escript P15.beam 3 a b c
  ["a","a","a","b","b","b","c","c","c"]

===============================================================================

16. Drop every N-th element from a list

  $ caramel compile --debug p16.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p16.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p16.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p16.ml.parsetree
  caramel: [DEBUG] OK
  File "p16.ml", line 17, characters 9-60:
  17 | let main (n :: xs) = format "~p\n" [ drop (parse_int n) xs ]
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  caramel: [DEBUG] Writing p16.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p16.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p16.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P16.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P16.core

  $ escript P16.beam 7 a b c d e f g h i
  ["a","b","c","d","e","f","h","i"]

===============================================================================

17. Split a list into two parts; the length of the first part is given.

  $ caramel compile --debug p17.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p17.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
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
  caramel: [DEBUG] Writing P17.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P17.core

  $ escript P17.beam 2 a b c d e f g
  {["a","b"],["c","d","e","f","g"]}

===============================================================================

18. Extract a slice from a list.

  $ caramel compile --debug p18.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p18.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
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
  caramel: [DEBUG] Writing P18.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P18.core

  $ escript P18.beam 2 6 a b c d e f g h i j k l m n
  ["c","d","e","f","g"]

===============================================================================

19. Rotate a list N places to the left.

  $ caramel compile --debug p19.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p19.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p19.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p19.ml.parsetree
  caramel: [DEBUG] OK
  File "p19.ml", line 34, characters 9-62:
  34 | let main (n :: xs) = format "~p\n" [ rotate xs (parse_int n) ]
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  caramel: [DEBUG] Writing p19.ml.lambda
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
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p19.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p19.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P19.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P19.core

  $ escript P19.beam 3 a b c d e f g h
  ["d","e","f","g","h","a","b","c"]

===============================================================================

20. Remove the K-th element from a list.

  $ caramel compile --debug p20.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p20.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p20.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p20.ml.parsetree
  caramel: [DEBUG] OK
  File "p20.ml", line 23, characters 9-65:
  23 | let main (n :: xs) = format "~p\n" [ remove_at (parse_int n) xs ]
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  caramel: [DEBUG] Writing p20.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p20.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p20.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P20.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P20.core

  $ escript P20.beam 2 a b c d
  ["a","b","d"]

===============================================================================

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
  caramel: [DEBUG] Writing P21.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P21.core

  $ escript P21.beam alfa 3 a b c d 
  ["a","b","c","alfa","d"]

===============================================================================

22. Create a list containing all integers within a given range. (easy)

  $ caramel compile --debug p22.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p22.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
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
  caramel: [DEBUG] Writing P22.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P22.core

  $ escript P22.beam 4 9 
  [4,5,6,7,8,9]

===============================================================================

23. Extract a given number of randomly selected elements from a list. (medium)

  $ caramel compile --debug p23.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p23.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
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
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
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
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] Writing p23.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p23.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p23.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P23.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P23.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P23.core

  $ escript P23.beam 3 a b c d e f g h
  ["f","b","e"]

===============================================================================

24. Lotto: Draw N different random numbers from the set 1..M. (easy)

  $ caramel compile --debug p24.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p24.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
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
  caramel: [DEBUG] Writing P24.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P24.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P24.core

  $ escript P24.beam 6 49
  [26,18,35,5,30,38]

===============================================================================

25. Generate a random permutation of the elements of a list. (easy)

  $ caramel compile --debug p25.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p25.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
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
  caramel: [DEBUG] Writing P25.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P25.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P25.core

  $ escript P25.beam a b c d e f 
  ["c","a","f","e","d"]

===============================================================================

26. Generate the combinations of K distinct objects chosen from the N elements of a list. (medium)

  $ caramel compile --debug p26.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p26.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
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
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p26.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p26.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p26.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p26.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P26.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P26.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P26.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P26.core

  $ escript P26.beam 2 a b c d
  [["a","b"],["a","c"],["a","d"],["b","c"],["b","d"],["c","d"]]

===============================================================================

27. Group the elements of a set into disjoint subsets. (medium)

  $ caramel compile --debug p27.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p27.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
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
  caramel: [DEBUG] list
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p27.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p27.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p27.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p27.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P27.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P27.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P27.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P27.core

  $ escript P27.beam 2 1 a b c d
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

===============================================================================

28. Sorting a list of lists according to length of sublists. (medium)

  $ caramel compile --debug p28.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p28.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p28.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p28.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p28.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p28.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p28.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p28.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p28.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P28.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P28.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P28.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P28.core

  $ escript P28.beam
  [[<<"i">>,<<"j">>,<<"k">>,<<"l">>],
   [<<"f">>,<<"g">>,<<"h">>],
   [<<"a">>,<<"b">>,<<"c">>],
   [<<"m">>,<<"n">>],
   [<<"d">>,<<"e">>],
   [<<"d">>,<<"e">>],
   [<<"o">>]]

===============================================================================

31. Determine whether a given integer number is prime. (medium)

  $ caramel compile --debug p31.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p31.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p31.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p31.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p31.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p31.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p31.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p31.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p31.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P31.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P31.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P31.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P31.core

  $ escript P31.beam
  [false,false,false]

===============================================================================

32. Determine the greatest common divisor of two positive integer numbers. (medium)

  $ caramel compile --debug p32.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p32.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p32.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p32.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p32.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p32.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p32.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p32.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p32.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P32.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P32.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P32.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P32.core

  $ escript P32.beam
  [{<<"gcd 13 27">>,1},{<<"gcd 20536 7826">>,2}]

===============================================================================

33. Determine whether two positive integer numbers are coprime. (easy)

  $ caramel compile --debug p33.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p33.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p33.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p33.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p33.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p33.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p33.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p33.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p33.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P33.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P33.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P33.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P33.core

  $ escript P33.beam
  [{<<"coprime 13 27">>,true},{<<"coprime 20536 7826">>,false}]

===============================================================================

34. Calculate Eulers totient function φ(m). (medium)

  $ caramel compile --debug p34.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p34.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p34.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p34.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p34.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p34.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p34.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p34.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p34.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P34.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P34.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P34.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P34.core

  $ escript P34.beam
  [{<<"phi 10">>,4},{<<"phi 13">>,12}]

===============================================================================

35. Determine the prime factors of a given positive integer. (medium)

  $ caramel compile --debug p35.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p35.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p35.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p35.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p35.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p35.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p35.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p35.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p35.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P35.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P35.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P35.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P35.core

  $ escript P35.beam
  {<<"factors 315">>,[3,3,5,7]}

===============================================================================

36. Determine the prime factors of a given positive integer (2). (medium)

  $ caramel compile --debug p36.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p36.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p36.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p36.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p36.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p36.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p36.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p36.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p36.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P36.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P36.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P36.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P36.core

  $ escript P36.beam
  {<<"factors 315">>,[{3,2},{5,1},{7,1}]}

===============================================================================

37. Calculate Eulers totient function φ(m) (improved). (medium)

  $ caramel compile --debug p37.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p37.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p37.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p37.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p37.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p37.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p37.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p37.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p37.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P37.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P37.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P37.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P37.core

  $ escript P37.beam
  [{<<"phi_improved 10">>,4},{<<"phi_improved 13">>,12}]

===============================================================================

39. A list of prime numbers. (easy)

  $ caramel compile --debug p39.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p39.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p39.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p39.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p39.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p39.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p39.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p39.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p39.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P39.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P39.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P39.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P39.core

  $ escript P39.beam
  [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,
   103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,
   199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,
   313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,
   433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,
   563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,
   673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,
   811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,
   941,947,953,967,971,977,983,991,997]

===============================================================================

40. Goldbachs conjecture. (medium)

  $ caramel compile --debug p40.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p40.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p40.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p40.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p40.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p40.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p40.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p40.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p40.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P40.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P40.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P40.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P40.core

  $ escript P40.beam
  {<<"goldbach 28">>,{5,23}}

===============================================================================

41. A list of Goldbach compositions. (medium)

  $ caramel compile --debug p41.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p41.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p41.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p41.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p41.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p41.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p41.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p41.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p41.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P41.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P41.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P41.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P41.core

  $ escript P41.beam
  [{<<"goldbach_list 9 20">>,
    [{10,{3,7}},{12,{5,7}},{14,{3,11}},{16,{3,13}},{18,{5,13}},{20,{3,17}}]},
   {<<"goldbach_limit 1 2000 50">>,
    [{992,{73,919}},{1382,{61,1321}},{1856,{67,1789}},{1928,{61,1867}}]}]

===============================================================================

46 & 47. Truth tables for logical expressions (2 variables). (medium)

  $ caramel compile --debug p46.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p46.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p46.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p46.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p46.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Var/1
  caramel: [DEBUG] constructor field access Not/1
  caramel: [DEBUG] constructor field access And/2
  caramel: [DEBUG] constructor field access And/2
  caramel: [DEBUG] constructor field access Or/2
  caramel: [DEBUG] constructor field access Or/2
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p46.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p46.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p46.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p46.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P46.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P46.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P46.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P46.core

  $ escript P46.beam
  [{true,true,true},{true,false,true},{false,true,false},{false,false,false}]

===============================================================================

48. Truth tables for logical expressions. (medium)

  $ caramel compile --debug p48.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p48.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p48.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p48.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p48.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Var/1
  caramel: [DEBUG] constructor field access Not/1
  caramel: [DEBUG] constructor field access And/2
  caramel: [DEBUG] constructor field access And/2
  caramel: [DEBUG] constructor field access Or/2
  caramel: [DEBUG] constructor field access Or/2
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p48.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p48.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p48.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p48.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P48.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P48.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P48.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P48*.core

  $ escript P48.beam
  [{[{<<"a">>,true},{<<"b">>,true}],true},
   {[{<<"a">>,true},{<<"b">>,false}],true},
   {[{<<"a">>,false},{<<"b">>,true}],false},
   {[{<<"a">>,false},{<<"b">>,false}],false}]

===============================================================================

49. Gray code. (medium)

  $ caramel compile --debug p49.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p49.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p49.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p49.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p49.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p49.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p49.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p49.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p49.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P49.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P49.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P49.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P49.core

  $ escript P49.beam
  [[<<"0">>,<<"1">>],
   [<<"00">>,<<"01">>,<<"11">>,<<"10">>],
   [<<"000">>,<<"001">>,<<"011">>,<<"010">>,<<"110">>,<<"111">>,<<"101">>,
    <<"100">>]]

===============================================================================

50. Huffman code (hard)

SKIPPED: the implementation on the website has a mutable priority queue, and
we do NOT support mutable state, so I would have to rewrite it.

  $ # caramel compile --debug p50.ml

  $ # erlc P50.core

  $ # escript P50.beam

===============================================================================

55. Construct completely balanced binary trees. (medium)

  $ caramel compile --debug p55.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p55.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p55.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p55.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p55.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p55.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p55.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p55.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p55.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P55.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P55.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P55.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P55.core

  $ escript P55.beam
  [{node,<<"x">>,
         {node,<<"x">>,empty,empty},
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty}},
   {node,<<"x">>,
         {node,<<"x">>,empty,empty},
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty},
         {node,<<"x">>,empty,empty}},
   {node,<<"x">>,
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}},
         {node,<<"x">>,empty,empty}}]

===============================================================================

56. Symmetric binary trees. (medium)

  $ caramel compile --debug p56.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p56.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p56.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p56.ml.parsetree
  caramel: [DEBUG] OK
  File "p56.ml", lines 106-107, characters 2-52:
  106 | ..let (x :: y :: _) = cbal_tree 4 in
  107 |   format "~p\n" [ [is_symmetric x; is_symmetric y] ]
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  _::[]
  caramel: [DEBUG] Writing p56.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p56.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p56.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p56.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p56.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P56.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P56.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P56.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P56.core

  $ escript P56.beam
  [false,false]

===============================================================================

57. Binary search trees (dictionaries). (medium)

  $ caramel compile --debug p57.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p57.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p57.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p57.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p57.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p57.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p57.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p57.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p57.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P57.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P57.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P57.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P57.core

  $ escript P57.beam
  {node,3,
        {node,2,{node,1,empty,empty},empty},
        {node,5,empty,{node,7,empty,empty}}}

===============================================================================

58. Generate-and-test paradigm. (medium)

  $ caramel compile --debug p58.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p58.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p58.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p58.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p58.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p58.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p58.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p58.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p58.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P58.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P58.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P58.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P58.core

  $ escript P58.beam
  [{node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty},
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}},
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty}}]

===============================================================================

59. Construct height-balanced binary trees. (medium)

  $ caramel compile --debug p59.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p59.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p59.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p59.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p59.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p59.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p59.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p59.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p59.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P59.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P59.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P59.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P59.core

  $ escript P59.beam
  [{node,<<"x">>,
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}},
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}},
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty}},
   {node,<<"x">>,
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}},
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty},
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty},
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty},
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}},
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}},
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}},
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}},
         {node,<<"x">>,empty,empty}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty},
         {node,<<"x">>,empty,empty}},
   {node,<<"x">>,
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}},
         {node,<<"x">>,empty,empty}},
   {node,<<"x">>,
         {node,<<"x">>,empty,empty},
         {node,<<"x">>,empty,{node,<<"x">>,empty,empty}}},
   {node,<<"x">>,
         {node,<<"x">>,empty,empty},
         {node,<<"x">>,{node,<<"x">>,empty,empty},empty}},
   {node,<<"x">>,
         {node,<<"x">>,empty,empty},
         {node,<<"x">>,{node,<<"x">>,empty,empty},{node,<<"x">>,empty,empty}}}]

===============================================================================

SKIPPED: just seemed like too much to read.
60. Construct height-balanced binary trees with a given number of nodes. (medium)

  $ # caramel compile --debug p60.ml

  $ # erlc P60.core

  $ # escript P60.beam

===============================================================================

61. Count the leaves of a binary tree. (easy)

  $ caramel compile --debug p61.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p61.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p61.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p61.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p61.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p61.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p61.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p61.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p61.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P61.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P61.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P61.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P61.core

  $ escript P61.beam
  [0,3]

===============================================================================

61A. Collect the leaves of a binary tree in a list. (easy)

  $ caramel compile --debug p61A.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p61A.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p61A.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p61A.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p61A.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p61A.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p61A.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p61A.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p61A.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P61A.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P61A.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P61A.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P61A.core

  $ escript P61A.beam
  [[],[<<"d">>,<<"e">>,<<"g">>]]

===============================================================================

62. Collect the internal nodes of a binary tree in a list. (easy)

  $ caramel compile --debug p62.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p62.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p62.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p62.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p62.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p62.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p62.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p62.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p62.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P62.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P62.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P62.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P62.core

  $ escript P62.beam
  [[],[<<"b">>,<<"a">>,<<"c">>,<<"f">>]]

===============================================================================

62B. Collect the nodes at a given level in a list. (easy)

  $ caramel compile --debug p62a.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p62a.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p62a.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p62a.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p62a.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p62a.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p62a.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p62a.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p62a.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P62a.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P62a.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P62a.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P62a.core

  $ escript P62a.beam
  [[<<"b">>,<<"c">>],[]]

===============================================================================

63. Construct a complete binary tree. (medium)

  $ caramel compile --debug p63.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p63.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p63.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p63.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p63.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p63.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p63.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p63.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p63.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P63.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P63.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P63.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P63.core

  $ escript P63.beam
  {node,1,
        {node,2,{node,4,empty,empty},{node,5,empty,empty}},
        {node,3,{node,6,empty,empty},empty}}

===============================================================================

64. Layout a binary tree (1). (medium)

  $ caramel compile --debug p64.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p64.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p64.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p64.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p64.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] tuple
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p64.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p64.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p64.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p64.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P64.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P64.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P64.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P64.core

  $ escript P64.beam
  {node,{<<"n">>,8,1},
        {node,{<<"k">>,6,2},
              {node,{<<"c">>,2,3},
                    {node,{<<"a">>,1,4},empty,empty},
                    {node,{<<"h">>,5,4},
                          {node,{<<"g">>,4,5},
                                {node,{<<"e">>,3,6},empty,empty},
                                empty},
                          empty}},
              {node,{<<"m">>,7,3},empty,empty}},
        {node,{<<"u">>,12,2},
              {node,{<<"p">>,9,3},
                    empty,
                    {node,{<<"s">>,11,4},
                          {node,{<<"q">>,10,5},empty,empty},
                          empty}},
              empty}}

===============================================================================

65. Layout a binary tree (2). (medium)

  $ caramel compile --debug p65.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p65.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p65.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p65.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p65.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p65.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p65.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p65.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p65.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P65.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P65.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P65.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P65.core

  $ escript P65.beam
  {node,{<<"n">>,15,1},
        {node,{<<"k">>,7,2},
              {node,{<<"c">>,3,3},
                    {node,{<<"a">>,1,4},empty,empty},
                    {node,{<<"e">>,5,4},
                          {node,{<<"d">>,4,5},empty,empty},
                          {node,{<<"g">>,6,5},empty,empty}}},
              {node,{<<"m">>,11,3},empty,empty}},
        {node,{<<"u">>,23,2},
              {node,{<<"p">>,19,3},empty,{node,{<<"q">>,21,4},empty,empty}},
              empty}}

===============================================================================

66. Layout a binary tree (3). (hard)

  $ caramel compile --debug p66.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p66.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p66.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p66.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p66.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 2
  caramel: [DEBUG] tuple
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 2
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] indexed field access 2
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p66.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p66.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p66.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p66.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P66.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P66.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P66.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P66.core

  $ escript P66.beam
  {node,{<<"n">>,5,1},
        {node,{<<"k">>,3,2},
              {node,{<<"c">>,2,3},
                    {node,{<<"a">>,1,4},empty,empty},
                    {node,{<<"e">>,3,4},
                          {node,{<<"d">>,2,5},empty,empty},
                          {node,{<<"g">>,4,5},empty,empty}}},
              {node,{<<"m">>,4,3},empty,empty}},
        {node,{<<"u">>,7,2},
              {node,{<<"p">>,6,3},empty,{node,{<<"q">>,7,4},empty,empty}},
              empty}}

===============================================================================

67. A string representation of binary trees. (medium)

  $ caramel compile --debug p67.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p67.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p67.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p67.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p67.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p67.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p67.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p67.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p67.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p67.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P67.Binary.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P67.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P67.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P67.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P67.core

  $ escript P67.beam
  <<"a(b(d,e),c(,f(g,)))">>

===============================================================================

68. Preorder and inorder sequences of binary trees. (medium)

  $ caramel compile --debug p68.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p68.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p68.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p68.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p68.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] constructor field access Node/3
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] construct Node
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p68.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p68.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p68.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p68.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p68.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P68.Binary.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P68.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P68.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P68.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P68.core

  $ escript P68.beam
  [[1,2],[1,2]]

===============================================================================

70C. Count the nodes of a multiway tree. (easy)

  $ caramel compile --debug p70c.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p70c.ml)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p70c.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p70c.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p70c.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access T/2
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p70c.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p70c.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p70c.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p70c.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p70c.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P70c.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P70c.Binary.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P70c.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P70c.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P70c.core

  $ escript P70c.beam
  2

===============================================================================

SKIPPED: need to rewrite it to not use mutable string buffers.
70. Tree construction from a node string. (medium)

  $ # caramel compile --debug p70.ml

  $ # erlc P70.core

  $ # escript P70.beam

===============================================================================

71. Determine the internal path length of a tree. (easy)

  $ caramel compile --debug p71.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p71.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p71.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p71.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p71.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access T/2
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p71.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p71.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p71.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p71.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p71.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P71.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P71.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P71.Binary.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P71.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P71.core

  $ escript P71.beam
  [1,9]

===============================================================================

72. Construct the bottom-up order sequence of the tree nodes. (easy)

  $ caramel compile --debug p72.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p72.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p72.ml) (source_kind impl))
  
  caramel: [DEBUG] Writing p72.ml.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p72.ml.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] constructor field access T/2
  caramel: [DEBUG] list
  caramel: [DEBUG] constructor field access T/2
  caramel: [DEBUG] tuple
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] tuple
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] list car/cdr
  caramel: [DEBUG] tuple
  caramel: [DEBUG] indexed field access 1
  caramel: [DEBUG] indexed field access 0
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] list
  caramel: [DEBUG] Writing p72.ml.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing p72.ml.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p72.ml.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p72.ml.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing p72.ml.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P72.List.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P72.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P72.Binary.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing P72.Random.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ erlc P72.core

  $ escript P72.beam
  [[<<"b">>,<<"a">>],[<<"g">>,<<"f">>,<<"c">>,<<"d">>,<<"e">>,<<"b">>,<<"a">>]]

===============================================================================

SKIPPED: needs mutable buffers.
73. Lisp-like tree representation. (medium)

  $ caramel compile --debug p73.ml

  $ erlc P73.core

  $ escript P73.beam

===============================================================================

81. Path from one node to another one. (medium)

  $ # caramel compile --debug p81.ml

  $ # erlc P81.core

  $ # escript P81.beam

===============================================================================

82. Cycle from a given node. (easy)

  $ # caramel compile --debug p82.ml

  $ # erlc P82.core

  $ # escript P82.beam

===============================================================================

83. Construct all spanning trees. (medium)

  $ # caramel compile --debug p83.ml

  $ # erlc P83.core

  $ # escript P83.beam

===============================================================================

84. Construct the minimal spanning tree. (medium)

  $ # caramel compile --debug p84.ml

  $ # erlc P84.core

  $ # escript P84.beam

===============================================================================

85. Graph isomorphism. (medium)

  $ # caramel compile --debug p85.ml

  $ # erlc P85.core

  $ # escript P85.beam

===============================================================================

87. Depth-first order graph traversal. (medium)

  $ # caramel compile --debug p87.ml

  $ # erlc P87.core

  $ # escript P87.beam

===============================================================================

91. Eight queens problem. (medium)

  $ # caramel compile --debug p91.ml

  $ # erlc P91.core

  $ # escript P91.beam

===============================================================================

95. English number words. (medium)

  $ # caramel compile --debug p95.ml

  $ # erlc P95.core

  $ # escript P95.beam

===============================================================================

96. Syntax checker. (medium)

  $ # caramel compile --debug p96.ml

  $ # erlc P96.core

  $ # escript P96.beam

===============================================================================

97. Sudoku. (medium)

  $ # caramel compile --debug p97.ml

  $ # erlc P97.core

  $ # escript P97.beam

===============================================================================

98. Nonograms. (hard)

  $ # caramel compile --debug p98.ml

  $ # erlc P98.core

  $ # escript P98.beam
