open Sexplib.Std

(* TODO(@ostera): turn these back from `string` to `Fpath.t` *)
type opts = { sources : string list; stdlib : string option; dump_ast : bool }
[@@deriving sexp]

let handle_typedtree ~opts ~unit ~module_name ~signature ~structure =
  let open Caramel_sugarcane in
  let asts =
    Sugarcane.translate
      {
        file_name = Compilation_unit.file_name unit;
        module_name;
        signature;
        structure;
      }
  in

  if opts.dump_ast then Output.write_files ~kind:`ast asts else ();
  Output.write_files ~kind:`sources asts

let run_one ~opts ~caml source =
  match Compilation_unit.from_source source with
  | Error err -> Error (`Compilation_error err)
  | Ok unit -> (
      match Compilation_unit.source_kind unit with
      | Interface -> Caml_compiler.compile_interface ~unit caml
      | Implementation ->
          Caml_compiler.compile_implementation ~unit (handle_typedtree ~opts)
            caml)

let compile_all ~opts ~caml ~sources =
  match
    let _ = List.map (run_one ~opts ~caml) sources in
    Warnings.check_fatal ()
  with
  | exception Env.Error err ->
      Env.report_error Format.std_formatter err;
      Format.fprintf Format.std_formatter "\n%!";
      Error `Compilation_error
  | exception exc ->
      (match Location.error_of_exn exc with
      | Some (`Ok error) -> Location.print_report Format.std_formatter error
      | _ ->
          Format.fprintf Format.std_formatter "ERROR: %s\n"
            (Printexc.to_string exc));
      Error `Other_error
  | _ -> Ok ()

let run ({ sources; stdlib; _ } as opts) =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);

  Logs.debug (fun f ->
      f "Running Sugarcane compiler on sources: \n%s\n"
        (Sexplib.Sexp.to_string_hum ~indent:2 (sexp_of_opts opts)));

  let sources = List.map Fpath.v sources in

  let caml =
    Caml_compiler.init
      {
        stdlib = Option.map Fpath.v stdlib;
        tool_name = "caramel";
        opened_modules = [];
        include_dirs = [];
      }
  in

  compile_all ~opts ~caml ~sources
