let () =
  let ocaml_version =
    Scanf.sscanf Sys.ocaml_version "%u.%u" (fun a b -> (a, b))
  in
  let filename =
    if ocaml_version >= (4, 12) then
      "ge_412.ml"
    else
      "ge_408.ml"
  in
  print_string filename
