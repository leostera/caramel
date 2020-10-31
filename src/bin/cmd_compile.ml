open Cmdliner

let name = "compile"

let doc =
  "Compile OCaml to idiomatic Erlang, OCaml to Core Erlang, or Erlang to a \
   Binary."

let description =
  {| The Caramel compiler can take as input OCaml and
  Erlang files, and will be able to compile them to either Erlang or Core
  Erlang sources, or a binary.

  OCaml inputs (.ml and .mli files) will be compiled to .erl and .core files
  Erlang inputs (.erl files) will be compiled to an .exe binary
  |}

let info = Info.make ~name ~doc ~description

let run sources dump_ast no_stdlib stdlib_path targets =
  match
    Caramel_compiler.Compiler.compile
      { sources; dump_ast; targets; no_stdlib; stdlib_path }
  with
  | Ok () -> 0
  | Error _ -> 1

let cmd =
  let sources =
    Arg.(
      non_empty & pos_all string []
      & info [] ~docv:"SOURCES" ~doc:"A list of source files to compile")
  in
  let targets =
    let open Caramel_compiler.Compiler.Target in
    let targets =
      Arg.enum
        [
          ("core", Core_erlang);
          ("erlang", Erlang);
          ("native", Native);
          ("archive", Archive);
        ]
    in
    Arg.(
      value
      & opt_all ~vopt:Erlang targets [ Erlang ]
      & info [ "t"; "target" ] ~docv:"TARGET"
          ~doc:
            "The compilation target for this set of units, can be specified \
             multiple times. If an input source that cannot be compiled to \
             this target is found, compilation will end abruptly.\n\n\
             Valid targets are: core, for compiling OCaml code into Core \
             Erlang; erlang, for compiling OCaml code into Erlang; native, for \
             compiling Erlang code into native binaries. Choose wisely.")
  in
  ( Term.(
      pure run $ sources $ Common_flags.dump_ast $ Common_flags.no_stdlib
      $ Common_flags.stdlib_path $ targets),
    info )
