open Cmdliner

let name = "check"

let doc = "Typecheck Erlang and Core Erlang sources"

let description =
  {| The Caramel compiler can take as input Erlang and
  Core Erlang files and it will type check them.
  |}

let info = Info.make ~name ~doc ~description

let run sources dump_ast no_stdlib stdlib_path =
  let open Caramel_compiler.Compiler.Target in
  Caramel_compiler.Compiler.compile
    { sources; dump_ast; no_stdlib; targets = [ Type_check ]; stdlib_path }

let cmd =
  let sources =
    Arg.(
      non_empty & pos_all string []
      & info [] ~docv:"SOURCES" ~doc:"A list of source files to type-check")
  in
  ( Term.(
      pure run $ sources $ Common_flags.dump_ast $ Common_flags.no_stdlib
      $ Common_flags.stdlib_path),
    info )
