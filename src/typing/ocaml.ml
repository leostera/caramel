let tool_name = "caramelc-check"

(* Verbatim copied from ocaml/driver/optmain.ml *)
module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'

  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx

  let import_symbol = Import_approx.import_symbol

  let size_int = Arch.size_int

  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end

let backend = (module Backend : Backend_intf.S)

let interface ~source_file ~output_prefix =
  Compile_common.with_info ~native:false ~tool_name ~source_file ~output_prefix
    ~dump_ext:"cmx"
  @@ fun i ->
  let parse = Pparse.parse_interface ~tool_name i.source_file in
  let typed = Typemod.type_interface i.env parse in
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  `Signature typed

let implementation ~source_file ~output_prefix =
  Compile_common.with_info ~native:false ~tool_name ~source_file ~output_prefix
    ~dump_ext:"cmx"
  @@ fun i ->
  let parse = Pparse.parse_implementation ~tool_name i.source_file in
  let typed, _coercion =
    Typemod.type_implementation i.source_file i.output_prefix i.module_name
      i.env parse
  in
  `Structure typed
