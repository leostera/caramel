Calling compile without inputs will error out.
  $ caramelc compile
  caramelc: required argument SOURCES is missing
  Usage: caramelc compile [OPTION]... SOURCES...
  Try `caramelc compile --help' or `caramelc --help' for more information.
  [1]

Calling compile with uncompilable files will error out.
  $ caramelc compile dummy.txt
  Attempted to compile dummy.txt, but .txt files are not supported with the target flag: --target=erl
  [1]
