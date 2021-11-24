type opts = { sources : Fpath.t list; stdlib : Fpath.t option }

let handle_typed_tree ~module_name ~signature typedtree =
  Sugarcane.translate ~module_name ~signature typedtree
  |> Erlang.Printer.to_sources

let run_one source =
  match Compilation_unit.from_source source with
  | Error err -> Error (`Compilation_error err)
  | Ok unit -> (
      match unit.source_kind with
      | Interface -> Caml_compiler.compile_interface ~unit
      | Implementation ->
          Caml_compiler.compile_implementation ~unit ~handle_typed_tree)

let run { sources; stdlib; _ } =
  Caml_compiler.init { stdlib };
  let results = List.map run_one sources in
  (* TODO(@ostera): move the file on unit.target_file *)
  handle_errors results
