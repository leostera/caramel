open Comp_misc.Opts
module Dependency_sorter = Comp_misc.Dependency_sorter
module Source_tagger = Comp_misc.Source_tagger
module Target = Comp_misc.Target

let tool_name = "caramelc"

let default_stdlib_path =
  Filename.concat (Filename.dirname Sys.executable_name) "stdlib"

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

let initialize_compiler ~opts =
  Clflags.nopervasives := true;
  Clflags.no_std_include := true;
  Clflags.open_modules := if opts.no_stdlib then [] else [ "Stdlib"; "Beam" ];
  Clflags.include_dirs :=
    if opts.no_stdlib then []
    else
      [
        Filename.concat opts.stdlib_path "ocaml";
        Filename.concat opts.stdlib_path "beam";
      ];
  Compmisc.init_path ();
  let _ = Compmisc.initial_env () in
  ()

let compile_one source ~target ~opts =
  let open Source_tagger in
  let open Target in
  let fn, source_file =
    match (source, target) with
    | Mli file, _ -> (Optcompile.interface, file)
    | Ml file, Archive -> (Ocaml_archive.archive ~opts, file)
    | Ml file, Erlang -> (Ocaml_to_erlang.compile ~opts, file)
    | Ml file, Core_erlang -> (Ocaml_to_core_erlang.compile ~opts, file)
    | Ml file, _ -> (Optcompile.implementation ~backend, file)
    | Erl file, Native | Erl file, Type_check ->
        (Erlang_to_native.compile ~opts, file)
    | Erl file, t -> raise (Unsupported_file_type_for_target (t, file, ".erl"))
    | Other (t, file, ext), _ ->
        raise (Unsupported_file_type_for_target (t, file, ext))
  in
  fn ~source_file ~output_prefix:(Filename.chop_extension source_file)

let compile ({ sources; target; _ } as opts) =
  match
    initialize_compiler ~opts;
    Source_tagger.prepare ~sources ~target
    |> List.iter (compile_one ~target ~opts);
    Warnings.check_fatal ()
  with
  | exception Env.Error err ->
      Env.report_error Format.std_formatter err;
      Format.fprintf Format.std_formatter "\n%!"
  | exception exc -> (
      match Location.error_of_exn exc with
      | Some (`Ok error) -> Location.print_report Format.std_formatter error
      | _ ->
          Format.fprintf Format.std_formatter "ERROR: %s\n"
            (Printexc.to_string exc) )
  | _ -> ()
