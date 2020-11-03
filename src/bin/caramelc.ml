open Cmdliner

module Help = struct
  let info name =
    let doc = "Caramel compiler" in
    let description = "Caramel is a compiler from OCaml to Erlang." in
    Info.make ~name ~doc ~description

  let cmd = (Term.(ret (const (`Help (`Pager, None)))), info "caramelc")
end

let run () =
  let cmds =
    [
      Cmd_compile.cmd;
      Cmd_sort_deps.cmd;
      Cmd_check.cmd;
      Cmd_parse.cmd;
      Cmd_verify.cmd;
    ]
  in
  Term.(exit_status @@ eval_choice Help.cmd cmds)
