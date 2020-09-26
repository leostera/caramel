open Cmdliner

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

  let run sources dump_ast =
    Caramel_compiler.Compiler.compile { sources; dump_ast }

  let cmd =
    let sources =
      Arg.(non_empty & pos_all string [] & info [] ~docv:"SOURCES")
    in
    let dump_ast =
      Arg.(value & flag & info [ "d"; "dump-ast" ] ~docv:"DUMP_AST")
    in
    (Term.(pure run $ sources $ dump_ast), info)
end

module Sort_deps = struct
  let name = "sort-deps"

  let doc = "Sort OCaml files by their dependencies on each other."

  let description = {||}

  let info = Info.make ~name ~doc ~description

  let run sources =
    sources
    |> Caramel_compiler.Dependency_sorter.print_sorted_files

  let args =
    let sources = Arg.(non_empty & pos_all file [] & info [] ~docv:"SOURCES") in
    sources

  let cmd = (Term.(pure run $ args), info)
end

(*
module Typecheck = struct
  let name = "check"
  let doc = "Typecheck Erlang and Core Erlang sources"
  let description = {| The Caramel compiler can take as input Erlang and
  Core Erlang files and it will type check them.
  |}
  let info = Info.make ~name ~doc ~description

  let run _sources = ()

  let args =
    let sources = Arg.(non_empty & pos_all file [] & info [] ~docv:"SOURCES") in
    sources

  let cmd = Term.(pure run), info
end
*)

module Help = struct
  let info name =
    let doc = "Caramel compiler" in
    let description = "Caramel is a compiler from OCaml to Erlang." in
    Info.make ~name ~doc ~description

  let cmd = (Term.(ret (const (`Help (`Pager, None)))), info "caramelc")
end

let _ =
  let cmds = [ Compile.cmd; Sort_deps.cmd ] in
  Term.(exit @@ eval_choice Help.cmd cmds)
