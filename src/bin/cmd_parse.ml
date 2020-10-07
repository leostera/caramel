open Cmdliner

let name = "parse"

let doc = "Helper command to parse sources and dump ASTs"

let description =
  {| The Caramel compiler can take as input Erlang, Core Erlang, and OCaml files.
  |}

let info = Info.make ~name ~doc ~description

let run sources =
  List.iter
    (fun source ->
      ( match Erlang.Parse.from_file source with
      | Ok ast ->
          Sexplib.Sexp.pp_hum_indent 2 Format.std_formatter
            (Erlang.Ast.sexp_of_t ast)
      | Error (`Parser_error err) ->
          Format.fprintf Format.std_formatter "ERROR: %s" err );
      Format.fprintf Format.std_formatter "\n%!")
    sources

let cmd =
  let sources =
    Arg.(
      non_empty & pos_all string []
      & info [] ~docv:"SOURCES" ~doc:"A list of source files to parse")
  in
  (Term.(pure run $ sources), info)
