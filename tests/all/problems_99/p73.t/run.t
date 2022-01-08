SKIPPED: needs mutable buffers.
73. Lisp-like tree representation. (medium)

  $ caramel compile --debug p73.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (p73.ml)) (stdlib (./)) (dump_parsetree true) (dump_typedtree true)
    (dump_ir true) (dump_pass -1) (dump_erl_ast true) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file p73.ml) (source_kind impl))
  
  File "p73.ml", line 1:
  Error: I/O error: p73.ml: No such file or directory
  [1]

  $ erlc Caramel.P73.core
  Caramel.P73.core: open error 'no such file or directory'
  [1]

  $ escript Caramel.P73.beam
  escript: Failed to open file: Caramel.P73.beam
  [1]
