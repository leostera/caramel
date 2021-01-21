open Cmdliner

let no_stdlib =
  Arg.(
    value & flag
    & info [ "no-stdlib" ] ~docv:"NO_STDLIB"
        ~doc:
          "Use this flag to compile sources without opening the Standard \
           Library by default.")

let stdlib_path =
  Arg.(
    value
    & opt string Caramel_compiler.Compiler.default_stdlib_path
    & info [ "stdlib-path" ] ~env:(env_var "CARAMEL_STDLIB_PATH"))

let dump_ast =
  Arg.(
    value & flag
    & info [ "d"; "dump-ast" ] ~docv:"DUMP_AST"
        ~doc:
          "Use this flag to print out to standard output the ASTs of the \
           different representations being used during compilation. This is \
           NOT suitable for programmatic usage, and its mostly used for \
           debugging the compiler itself.")
