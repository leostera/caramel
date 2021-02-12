let run () =
  try Ok (Ocaml_lsp_server.run ~log_file:(Some "-"))
  with _ -> Error `Lsp_error
