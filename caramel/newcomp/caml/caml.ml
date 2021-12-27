open Caramel_misc

(* Re-exported modules *)
module Parsetree = Parsetree

let tool_name = "caramel"

(* Compiler API *)

let init = Caml_compiler.init

(* Helpers *)
let with_info ~unit kind =
  let output_prefix, dump_ext =
    match kind with
    | Source_kind.Interface -> (Compilation_unit.module_name unit, "cmi")
    | Source_kind.Implementation -> (Compilation_unit.module_name unit, "cmx")
  in
  Compile_common.with_info ~output_prefix ~dump_ext
    ~source_file:(Compilation_unit.source_file unit) ~native:false ~tool_name
    (fun info -> info)

(* Interface API *)

let parse_intf ~unit =
  match
    let info = with_info ~unit Source_kind.Interface in
    Compile_common.parse_intf info
  with
  | exception e -> Error (`Caml_parse_error e)
  | parsetree -> Ok parsetree

let typecheck_intf ~unit ~parsetree =
  match
    let info = with_info ~unit Source_kind.Interface in
    let typedtree = Compile_common.typecheck_intf info parsetree in
    Compile_common.emit_signature info parsetree typedtree;
    typedtree
  with
  | exception e -> Error (`Caml_typing_error e)
  | signature -> Ok signature

(* Implementation API *)

let parse_impl ~unit =
  match
    let info = with_info ~unit Source_kind.Implementation in
    Compile_common.parse_impl info
  with
  | exception e -> Error (`Caml_parse_error e)
  | parsetree -> Ok parsetree

let typecheck_impl ~unit ~parsetree =
  match
    let info = with_info ~unit Source_kind.Implementation in
    let typedtree = Compile_common.typecheck_impl info parsetree in
    let lambda =
      Translmod.transl_implementation_flambda info.module_name
        (typedtree.structure, typedtree.coercion)
    in
    let simp_lambda = Simplif.simplify_lambda lambda.code in
    Warnings.check_fatal ();

    (typedtree, Lambda.{ lambda with code = simp_lambda })
  with
  | exception e -> Error (`Caml_typing_error e)
  | program -> Ok program

(* Error API *)
let print_type_error err =
  match err with
  | Env.Error err ->
      Env.report_error Format.std_formatter err;
      Format.fprintf Format.std_formatter "\n%!"
  | exc -> (
      match Location.error_of_exn exc with
      | Some (`Ok error) -> Location.print_report Format.std_formatter error
      | _ ->
          Format.fprintf Format.std_formatter "ERROR: %s\n"
            (Printexc.to_string exc))

let print_parse_error exc =
  match Location.error_of_exn exc with
  | Some (`Ok error) -> Location.print_report Format.std_formatter error
  | _ ->
      Format.fprintf Format.std_formatter "ERROR: %s\n" (Printexc.to_string exc)
