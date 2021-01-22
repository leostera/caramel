open Comp_misc.Opts
open Compile_common

let tool_name = "caramel:lambda-to-core"

let compile ~source_file ~output_prefix ~opts =
  let backend info (typed, coercion) =
    let lambda =
      Translmod.transl_implementation info.module_name (typed, coercion)
    in
    let module_name = info.module_name in
    let core_ast = Ast_transl.from_lambda ~module_name lambda.code in
    if opts.dump_ast then (
      Sexplib.Sexp.pp_hum_indent 2 Format.std_formatter
        (Erlang.Core.Ast.sexp_of_t core_ast);
      Format.fprintf Format.std_formatter "\n\n%!");
    Erlang.Core.Printer.to_sources [ core_ast ]
  in
  Compile_common.with_info ~native:false ~tool_name ~source_file ~output_prefix
    ~dump_ext:"cmo"
  @@ fun info -> Compile_common.implementation info ~backend
