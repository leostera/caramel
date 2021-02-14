type opts = { debug : bool }

let run { debug } =
  try
    let log_file = if debug then Some "/tmp/caramel_lsp.log" else None in
    Ok (Ocaml_lsp_server.run ~log_file)
  with _ -> Error `Lsp_error
