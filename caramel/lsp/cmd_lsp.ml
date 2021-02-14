open Cmdliner

let name = "start"

let doc = "Start the Caramel LSP Server"

let description = {| Start the Caramel LSP Server |}

let info = Info.make ~name ~doc ~description

let run debug =
  match Caramel_lsp.Lsp.run { debug } with Ok () -> 0 | Error _ -> 1

let debug =
  Arg.(
    value & flag
    & info [ "debug" ] ~docv:"CARAMEL_LSP_DEBUG"
        ~doc:"Enable debug logging to a file.")

let cmd = (Term.(pure run $ debug), info)
