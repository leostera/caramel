open Cmdliner

let name = "fmt"

let doc = "Format Caramel code."

let description = {| Reformats Caramel source code. |}

let info = Info.make ~name ~doc ~description

let run sources =
  let sources = List.map Fpath.v sources in
  match Caramel_formatter.Formatter.format { sources; action = In_place } with
  | Ok () -> 0
  | Error _ -> 1

let cmd =
  let sources =
    Arg.(
      non_empty & pos_all string []
      & info [] ~docv:"SOURCES" ~doc:"A list of source files to format")
  in
  (Term.(pure run $ sources), info)
