open Cmdliner

module Common_flags = struct
  let no_stdlib = Arg.(value & flag & info [ "no-stdlib" ] ~docv:"NO_STDLIB")

  let stdlib_path =
    Arg.(
      value
      & opt string Caramel_compiler.Compiler.default_stdlib_path
      & info [ "stdlib-path" ] ~env:(env_var "CARAMELC_STDLIB_PATH"))

  let dump_ast =
    Arg.(
      value & flag
      & info [ "d"; "dump-ast" ] ~docv:"DUMP_AST"
          ~doc:
            "Use this flag to print out to standard output the ASTs of the \
             different representations being used during compilation. This is \
             NOT suitable for programmatic usage, and its mostly used for \
             debugging the compiler itself.")
end

module Compile = struct
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

  let run sources dump_ast no_stdlib stdlib_path target =
    Caramel_compiler.Compiler.compile
      { sources; dump_ast; target; no_stdlib; stdlib_path }

  let cmd =
    let sources =
      Arg.(
        non_empty & pos_all string []
        & info [] ~docv:"SOURCES" ~doc:"A list of source files to compile")
    in
    let target =
      let open Caramel_compiler.Compiler.Target in
      let targets =
        Arg.enum
          [
            ("core", Core_erlang);
            ("erl", Erlang);
            ("native", Native);
            ("archive", Archive);
          ]
      in
      Arg.(
        value
        & opt targets ~vopt:Erlang Erlang
        & info [ "t"; "target" ] ~docv:"TARGET"
            ~doc:
              "The compilation target for this set of units. If an input \
               source that cannot be compiled to this target is found, \
               compilation will end abruptly.\n\n\
               Valid targets are: core, for compiling OCaml code into Core \
               Erlang; erl, for compiling OCaml code into Erlang; native, for \
               compiling Erlang code into native binaries. Choose wisely.")
    in
    ( Term.(
        pure run $ sources $ Common_flags.dump_ast $ Common_flags.no_stdlib
        $ Common_flags.stdlib_path $ target),
      info )
end

module Sort_deps = struct
  let name = "sort-deps"

  let doc = "Sort OCaml files by their dependencies on each other."

  let description = {||}

  let info = Info.make ~name ~doc ~description

  let run sources =
    Caramel_compiler.Compiler.Dependency_sorter.print_sorted_files sources

  let args =
    let sources = Arg.(non_empty & pos_all file [] & info [] ~docv:"SOURCES") in
    sources

  let cmd = (Term.(pure run $ args), info)
end

module Typecheck = struct
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
      { sources; dump_ast; no_stdlib; target = Type_check; stdlib_path }

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
end

module Help = struct
  let info name =
    let doc = "Caramel compiler" in
    let description = "Caramel is a compiler from OCaml to Erlang." in
    Info.make ~name ~doc ~description

  let cmd = (Term.(ret (const (`Help (`Pager, None)))), info "caramelc")
end

let run () =
  let cmds = [ Compile.cmd; Sort_deps.cmd; Typecheck.cmd ] in
  Term.(exit @@ eval_choice Help.cmd cmds)
