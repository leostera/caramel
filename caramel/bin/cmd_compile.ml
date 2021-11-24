open Cmdliner

let name = "compile"

let doc = "Compile Caramel code to run on the Erlang VM."

let description =
  {| The Caramel takes as input OCaml sources and compiles them to Erlang code.
  |}

let info = Info.make ~name ~doc ~description

let run sources dump_ast no_stdlib stdlib_path =
  match
    (*
    Caramel_compiler.Compiler.compile
      { sources; dump_ast; targets = [ Erlang ]; no_stdlib; stdlib_path }
      *)
    Caramel_newcomp.Newcomp.Runner.run
      { sources; stdlib = (if no_stdlib then None else Some stdlib_path) }
  with
  | Ok () -> 0
  | Error _ -> 1

let cmd =
  let sources =
    Arg.(
      non_empty & pos_all string []
      & info [] ~docv:"SOURCES" ~doc:"A list of source files to compile")
  in
  ( Term.(
      pure run $ sources $ Common_flags.dump_ast $ Common_flags.no_stdlib
      $ Common_flags.stdlib_path),
    info )
