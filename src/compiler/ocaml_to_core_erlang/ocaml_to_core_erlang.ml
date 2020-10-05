open Comp_misc.Opts
open Compile_common

let tool_name = "caramelc:lambda-to-core"

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

let compile ~source_file ~output_prefix ~opts =
  let backend info (typed, coercion) =
    let lambda = to_lambda info (typed, coercion) in
    let bytecode = to_bytecode info lambda in
    let _ = emit_bytecode info bytecode in
    let module_name = info.module_name in
    let core_ast = Ast_transl.from_lambda ~module_name lambda.code in
    if opts.dump_ast then (
      Sexplib.Sexp.pp_hum_indent 2 Format.std_formatter
        (Erlang.Core.Ast.sexp_of_t core_ast);
      Format.fprintf Format.std_formatter "\n\n%!" );
    Erlang.Core.Printer.to_sources [ core_ast ]
  in
  Compile_common.with_info ~native:false ~tool_name ~source_file ~output_prefix
    ~dump_ext:"cmo"
  @@ fun info -> Compile_common.implementation info ~backend
