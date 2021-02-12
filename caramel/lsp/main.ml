open Cmdliner

module Help = struct
  let info name =
    let doc = "Caramel Language Server" in
    let description =
      "Caramel is a functional language for building type-safe, scalable, and \
       maintainable applications."
    in
    Info.make ~name ~doc ~description

  let cmd = (Term.(ret (const (`Help (`Pager, None)))), info "caramel-lsp")
end

let _ = [ Cmd_lsp.cmd ] |> Term.eval_choice Help.cmd |> Term.exit_status
