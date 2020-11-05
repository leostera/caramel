open Compile_common

let tool_name = "caramelc:typing:erlang_as_ocaml"

let parse ~source_file ~dump_ast =
  let erlang_ast =
    match Erlang.Parse.from_file source_file with
    | exception exc ->
        Format.fprintf Format.std_formatter "Unhandled parsing error: %s"
          (Printexc.to_string exc);
        exit 1
    | Error (`Parser_error msg) ->
        Format.fprintf Format.std_formatter "Parser_error: %s" msg;
        exit 1
    | Ok structure -> Erlang.Ast_helper.Mod.of_structure structure
  in
  let parsetree = Erlang_to_native.Ast_transl.to_parsetree erlang_ast in
  if dump_ast then (
    Printast.structure 0 Format.std_formatter parsetree;
    Format.fprintf Format.std_formatter "\n\n%!" );
  parsetree

let check ~source_file ~output_prefix ~dump_ast =
  Compile_common.with_info ~native:false ~tool_name ~source_file ~output_prefix
    ~dump_ext:"cmo"
  @@ fun i ->
  let parsetree = parse ~source_file ~dump_ast in
  let (typed, _coercion) =
    parsetree
    |> Profile.(record typing)
         (Typemod.type_implementation i.source_file i.output_prefix
            i.module_name i.env)
  in
  `Structure typed
