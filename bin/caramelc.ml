let run () = ()

let info name =
  Info.make ~name ~doc:"Caramel compiler"
    ~description:
      "Caramel is a compiler from OCaml to Erlang."

let command_main = (Cmdliner.Term.(pure run), info "caramelc")

let compile = (Cmdliner.Term.(pure run), info "compile")

let _ =
  Cmdliner.Term.eval_choice command_main
    [
      compile
    ]
