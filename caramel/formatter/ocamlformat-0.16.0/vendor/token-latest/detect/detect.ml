let dir_name version =
  if version >= (4, 11) then "411"
  else if version >= (4, 8) then "408"
  else "406"

let () =
  Scanf.sscanf Sys.ocaml_version "%d.%d" (fun a b -> (a, b))
  |> dir_name |> print_string
