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
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  [ Cmd_compile.cmd; Cmd_version.cmd; Cmd_parse.cmd ]
  |> Term.eval_choice Help.cmd |> Term.exit_status
