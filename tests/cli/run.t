Calling compile without inputs will error out.
  $ caramel compile
  caramel: required argument SOURCES is missing
  Usage: caramel compile [OPTION]... SOURCES...
  Try `caramel compile --help' or `caramel --help' for more information.
  [1]

Calling compile with uncompilable files will error out.
  $ caramel compile dummy.txt
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (dummy.txt)) (stdlib (./)) (dump_parsetree false)
    (dump_typedtree false) (dump_ir false) (dump_pass -1) (dump_erl_ast false)
    (print_time false) (new_syntax false) (to_beam false))
  
  Invalid file extension: .txt
  [1]

  $ cat >a.ml <<EOF
  > let a () = 1
  > EOF
  $ caramel compile a.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (a.ml)) (stdlib (./)) (dump_parsetree false) (dump_typedtree false)
    (dump_ir false) (dump_pass -1) (dump_erl_ast false) (print_time false)
    (new_syntax false) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file a.ml) (source_kind impl))
  
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing Caramel.A.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done
