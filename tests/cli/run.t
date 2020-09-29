Calling compile without inputs will error out.
  $ caramelc compile
  caramelc: required argument SOURCES is missing
  Usage: caramelc compile [OPTION]... SOURCES...
  Try `caramelc compile --help' or `caramelc --help' for more information.
  [1]

Calling compile with uncompilable files will error out.
  $ caramelc compile dummy.txt
  Attempted to compile file: dummy.txt, but the extension .txt is not supported.
  
  Try with an .ml, .mli, or .erl file instead.
  
  [1]
