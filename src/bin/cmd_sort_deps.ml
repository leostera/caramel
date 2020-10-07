open Cmdliner

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
