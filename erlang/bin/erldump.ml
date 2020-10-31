open Cmdliner

module Info = struct
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
        `P "Copyright (C) 2020, Abstract Machines Lab Sweden AB";
        `P "Erlang is licensed under Apache License 2.0";
      ]
    in
    let version =
      match Version.git_version with
      | "" -> Version.s
      | v -> Printf.sprintf "%s+git-%s" Version.s v
    in
    Term.info name ~version ~doc ~man
end

module Parse = struct
  let name = "parse"

  let doc = "Helper command to parse sources and dump ASTs"

  let description = {| |}

  let info = Info.make ~name ~doc ~description

  let pp_erlang_parsetree source =
    match Erlang.Parse.from_file source with
    | Ok structure ->
        Sexplib.Sexp.pp_hum_indent 2 Format.std_formatter
          (Erlang.Ast.sexp_of_structure structure);
        Format.fprintf Format.std_formatter "\n%!";
        0
    | Error (`Parser_error err) ->
        Format.fprintf Format.std_formatter "ERROR: %s%!\n" err;
        Format.fprintf Format.std_formatter "\n%!";
        1

  let run sources =
    List.fold_left
      (fun exit_code src ->
        if exit_code = 0 then pp_erlang_parsetree src else exit_code)
      0 sources

  let cmd =
    let sources =
      Arg.(
        non_empty & pos_all string []
        & info [] ~docv:"SOURCES" ~doc:"A list of source files to parse")
    in
    (Term.(pure run $ sources), info)
end

module Help = struct
  let info name =
    let doc = "Erlang AST dump tool" in
    let description =
      "A small tool to inspect the concrete AST of Erlang sources"
    in
    Info.make ~name ~doc ~description

  let cmd = (Term.(ret (const (`Help (`Pager, None)))), info "erldump")
end

let () =
  let cmds = [ Parse.cmd ] in
  Term.(exit_status @@ eval_choice Help.cmd cmds)
