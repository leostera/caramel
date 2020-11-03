open Cmdliner

let name = "verify"

let doc = "Verify that a generated module is isomorphic to its source module."

let description = {||}

let info = Info.make ~name ~doc ~description

let run sources =
  match Caramel_verify.Verify.verify sources with Ok () -> 0 | Error _ -> 1

let cmd =
  let sources =
    Arg.(
      non_empty & pos_all string []
      & info [] ~docv:"SOURCES" ~doc:"A list of source files to compile")
  in
  (Term.(pure run $ sources), info)
