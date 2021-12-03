open Compile_common
module Ast_transl = Ast_transl

let tool_name = "caramel:ml-to-erl"

let read_signature info =
  let module_name = info.module_name in
  let cmi_file = module_name ^ ".cmi" in
  try
    let intf_file = Load_path.find_uncap cmi_file in
    let sign = Env.read_signature module_name intf_file in
    Some sign
  with Not_found -> None

let compile ~source_file ~output_prefix ~opts:_ =
  let open Typedtree in
  let backend info {structure;_} =
    let signature = read_signature info in
    let module_name = info.module_name in
    let erl_ast = Ast_transl.from_typedtree ~module_name ~signature structure in
    Erlang.Printer.to_sources erl_ast
  in
  Compile_common.with_info ~native:false ~tool_name ~source_file ~output_prefix
    ~dump_ext:"cmo"
  @@ fun info -> Compile_common.implementation info ~backend
