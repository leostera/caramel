open Cmdliner

let name = "parse"

let doc = "(UNSTABLE) Helper command to parse sources and dump ASTs"

let description =
  {| The Caramel compiler can take as input Erlang, Core Erlang, and OCaml files.
  |}

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

let pp_ocaml_parsetree source_file =
  let tool_name = "caramel-" ^ name in
  Clflags.dump_parsetree := true;
  Compile_common.with_info ~native:false ~tool_name ~source_file
    ~output_prefix:".none" ~dump_ext:"cmo" (fun info ->
      let parsetree = Compile_common.parse_impl info in
      ignore parsetree;
      0)

let pp_ocaml_typedtree ~stdlib_path source_file =
  let tool_name = "caramel-" ^ name in
  Compile_common.with_info ~native:false ~tool_name ~source_file
    ~output_prefix:".none" ~dump_ext:"cmo" (fun i ->
      try
        Caramel_compiler.Compiler.initialize_compiler ~stdlib_path ();
        Compile_common.parse_impl i
        |> Typemod.type_implementation i.source_file i.output_prefix
             i.module_name i.env
        |> Printtyped.implementation_with_coercion i.ppf_dump;
        0
      with Env.Error err ->
        Env.report_error i.ppf_dump err;
        1)

let pp_ocaml_to_erlang_parsetree ~stdlib_path source_file =
  let tool_name = "caramel-" ^ name in
  Compile_common.with_info ~native:false ~tool_name ~source_file
    ~output_prefix:".none" ~dump_ext:"cmo" (fun i ->
      Caramel_compiler.Compiler.initialize_compiler ~stdlib_path ();
      try
        let typed, _ =
          Compile_common.parse_impl i
          |> Typemod.type_implementation i.source_file i.output_prefix
               i.module_name i.env
        in
        let signature =
          Caramel_compiler.Compiler.Ocaml_to_erlang.read_signature i
        in
        typed
        |> Caramel_compiler.Compiler.Ocaml_to_erlang.Ast_transl.from_typedtree
             ~module_name:source_file ~signature
        |> List.iter (fun t ->
               Erlang.Ast.sexp_of_t t |> Sexplib.Sexp.pp_hum_indent 2 i.ppf_dump;
               Format.fprintf i.ppf_dump "\n\n%!");
        0
      with Env.Error err ->
        Env.report_error i.ppf_dump err;
        exit 1)

let run stdlib_path sources language tree =
  let parse =
    match (language, tree) with
    | `Erlang, _ -> pp_erlang_parsetree
    | `OCaml, `Parsetree -> pp_ocaml_parsetree
    | `OCaml, `Typedtree -> pp_ocaml_typedtree ~stdlib_path
    | `OCaml_to_erlang, _ -> pp_ocaml_to_erlang_parsetree ~stdlib_path
  in
  List.fold_left
    (fun exit_code src -> if exit_code = 0 then parse src else exit_code)
    0 sources

let cmd =
  let sources =
    Arg.(
      non_empty & pos_all string []
      & info [] ~docv:"SOURCES" ~doc:"A list of source files to parse")
  in
  let tree =
    let trees = Arg.enum [ ("parse", `Parsetree); ("typed", `Typedtree) ] in
    Arg.(
      value
      & opt ~vopt:`Parsetree trees `Parsetree
      & info [ "t"; "tree" ] ~docv:"tree" ~doc:"Which stage AST to print")
  in
  let language =
    let languages =
      Arg.enum
        [ ("erl", `Erlang); ("ml", `OCaml); ("ml-to-erl", `OCaml_to_erlang) ]
    in
    Arg.(
      value
      & opt ~vopt:`Erlang languages `Erlang
      & info [ "l"; "lang" ] ~docv:"language"
          ~doc:"The source language to parse")
  in
  (Term.(pure run $ Common_flags.stdlib_path $ sources $ language $ tree), info)
