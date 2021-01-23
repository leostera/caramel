open Cmdliner

let name = "version"

let doc = "Show version information."

let description = {| |}

let info = Info.make ~name ~doc ~description

let run () =
  Printf.printf "caramel v%s\n" Caramel_compiler.Compiler_version.s;
  0

let cmd = (Term.(pure run $ const ()), info)
