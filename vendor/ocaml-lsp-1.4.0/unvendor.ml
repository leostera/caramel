let rec rm_rf path =
  let stat = Unix.lstat path in
  match stat.st_kind with
  | S_DIR ->
    clear path;
    Unix.rmdir path
  | _ -> Unix.unlink path

and clear path =
  Sys.readdir path |> Array.iter (fun name -> rm_rf (Filename.concat path name))

let () = clear "./ocaml-lsp-server/vendor"
