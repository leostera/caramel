open Cmdliner

let name = "start"

let doc = "Start the Caramel LSP Server"

let description = {| Start the Caramel LSP Server |}

let info = Info.make ~name ~doc ~description

let run () = match Caramel_lsp.Lsp.run () with Ok () -> 0 | Error _ -> 1

let cmd = (Term.(pure run $ const ()), info)
