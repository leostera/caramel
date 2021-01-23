open Cmdliner

module Help = struct
  let info name =
    let doc = "Caramel compiler" in
    let description =
      "Caramel is a functional language for building type-safe, scalable, and \
       maintainable applications."
    in
    Info.make ~name ~doc ~description

  let cmd = (Term.(ret (const (`Help (`Pager, None)))), info "caramel")
end

let _ =
  [
    Cmd_compile.cmd;
    Cmd_sort_deps.cmd;
    Cmd_parse.cmd;
    Cmd_fmt.cmd;
    Cmd_version.cmd;
  ]
  |> Term.eval_choice Help.cmd |> Term.exit_status
