open Compile_common

let tool_name = "caramel:re-to-erl"

let setup_lexbuf ~parser source_file =
  let ic = open_in source_file in
  try
    seek_in ic 0;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf source_file;
    parser lexbuf
  with e ->
    Format.fprintf Format.std_formatter "Reason parser error: %s"
      (Printexc.to_string e);
    close_in_noerr ic;
    exit 1

let read_signature info =
  let module_name = info.module_name in
  let cmi_file = module_name ^ ".cmi" in
  try
    let intf_file = Load_path.find_uncap cmi_file in
    let sign = Env.read_signature module_name intf_file in
    Some sign
  with Not_found -> None

let parse_implementation source_file =
  let implementation lexbuf =
    lexbuf |> Reason_toolchain.RE.implementation
    |> Reason_toolchain.To_current.copy_structure
  in
  setup_lexbuf ~parser:implementation source_file

let parse_interface source_file =
  let interface lexbuf =
    lexbuf |> Reason_toolchain.RE.interface
    |> Reason_toolchain.To_current.copy_signature
  in
  setup_lexbuf ~parser:interface source_file

let interface ~source_file ~output_prefix ~opts:_ =
  Compile_common.with_info ~native:false ~tool_name ~source_file ~output_prefix
    ~dump_ext:"cmo"
  @@ fun info ->
  Profile.record_call source_file @@ fun () ->
  let parsetree = parse_interface source_file in
  if Clflags.(should_stop_after Compiler_pass.Parsing) then ()
  else
    let tsg = typecheck_intf info parsetree in
    if not !Clflags.print_types then emit_signature info parsetree tsg

let compile ~source_file ~output_prefix ~opts:_ =
  Compile_common.with_info ~native:false ~tool_name ~source_file ~output_prefix
    ~dump_ext:"cmo"
  @@ fun info ->
  let parsetree = parse_implementation source_file in
  let typed, _coercion =
    parsetree
    |> Profile.(record typing)
         (Typemod.type_implementation info.source_file info.output_prefix
            info.module_name info.env)
  in
  let module_name = info.module_name in
  let signature = read_signature info in
  let erl_ast =
    Ocaml_to_erlang.Ast_transl.from_typedtree ~module_name ~signature typed
  in
  Erlang.Printer.to_sources erl_ast
