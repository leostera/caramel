open Caramel_compiler
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
      `P "Copyright (C) 2020.";
      `P
        "caramel is free software, you can redistribute it and/or modify it \
         under the terms of the GNU Lesser General Public License as published \
         by the Free Software Foundation, with linking exception; either \
         version 2.1 of the License, or (at your option) any later version.";
    ]
  in
  let version =
    match Compiler_version.git_version with
    | "" -> Compiler_version.s
    | v -> Printf.sprintf "%s+git-%s" Compiler_version.s v
  in
  Term.info name ~version ~doc ~man
