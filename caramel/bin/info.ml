open Caramel_newcomp
open Cmdliner

let make ~name ~doc ~description =
  let man =
    [
      `S "DESCRIPTION";
      `P description;
      `S "SEE ALSO";
      `P "ocaml(1) erlang";
      `S "AUTHORS";
      `P "Leandro Ostera.";
      `S "LICENSE";
      `P "Copyright (C) 2020-present, Leandro Ostera";
    ]
  in
  let version =
    match Newcomp.Version.git_version with
    | "" -> Newcomp.Version.s
    | v -> Printf.sprintf "%s+git-%s" Newcomp.Version.s v
  in
  Term.info name ~version ~doc ~man
