open Compile_common
open Comp_misc.Opts
module Ast_transl = Ast_transl

let tool_name = "caramel:erlang_to_native"

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
  Compile_common.with_info ~native:false ~tool_name ~source_file ~output_prefix
    ~dump_ext:"cmo"
  @@ fun i ->
  let erlang_ast =
    match Erlang.Parse.from_file i.source_file with
    | exception exc ->
        Format.fprintf Format.std_formatter "Unhandled parsing error: %s"
          (Printexc.to_string exc);
        exit 1
    | Error (`Parser_error msg) ->
        Format.fprintf Format.std_formatter "Parser_error: %s" msg;
        exit 1
    | Ok structure -> Erlang.Ast_helper.Mod.of_structure structure
  in
  if opts.dump_ast then (
    Sexplib.Sexp.pp_hum_indent 2 Format.std_formatter
      (Erlang.Ast.sexp_of_t erlang_ast);
    Format.fprintf Format.std_formatter "\n\n%!" );
  let parsetree = Ast_transl.to_parsetree erlang_ast in
  if opts.dump_ast then (
    Pprintast.structure Format.std_formatter parsetree;
    Format.fprintf Format.std_formatter "\n\n%!" );
  let typedtree =
    parsetree
    |> Profile.(record typing)
         (Typemod.type_implementation i.source_file i.output_prefix
            i.module_name i.env)
  in
  let lambda = to_lambda i typedtree in
  let bytecode = to_bytecode i lambda in
  emit_bytecode i bytecode
