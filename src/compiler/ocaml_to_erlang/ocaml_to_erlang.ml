open Compile_common

let tool_name = "caramelc:ml-to-erl"

let to_bytecode i lambda =
  lambda
  |> Profile.(record ~accumulate:true generate)
       (fun Lambda.{ code = lambda; required_globals; _ } ->
         let simp_lambda = lambda |> Simplif.simplify_lambda in
         let bytecode =
           simp_lambda |> Bytegen.compile_implementation i.module_name
         in
         (bytecode, required_globals))

let to_lambda i (typedtree, coercion) =
  (typedtree, coercion)
  |> Profile.(record transl) (Translmod.transl_implementation i.module_name)

let emit_bytecode i (bytecode, required_globals) =
  let cmofile = i.output_prefix ^ ".cmo" in
  let oc = open_out_bin cmofile in
  Misc.try_finally
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> Misc.remove_file cmofile)
    (fun () ->
      bytecode
      |> Profile.(record ~accumulate:true generate)
           (Emitcode.to_file oc i.module_name cmofile ~required_globals))

let read_signature info =
  let module_name = info.module_name in
  let cmi_file = module_name ^ ".cmi" in
  try
    let intf_file = Load_path.find_uncap cmi_file in
    let sign = Env.read_signature module_name intf_file in
    Some sign
  with Not_found -> None

let compile ~source_file ~output_prefix ~opts:_ =
  let backend info (typed, coercion) =
    let lambda = to_lambda info (typed, coercion) in
    let bytecode = to_bytecode info lambda in
    let _ = emit_bytecode info bytecode in
    let signature = read_signature info in
    let module_name = info.module_name in
    let erl_ast = Ast_transl.from_typedtree ~module_name typed signature in
    Erlang.Printer.to_sources erl_ast
  in
  Compile_common.with_info ~native:false ~tool_name ~source_file ~output_prefix
    ~dump_ext:"cmo"
  @@ fun info -> Compile_common.implementation info ~backend
