open Comp_misc.Opts
module Dependency_sorter = Comp_misc.Dependency_sorter
module Source_tagger = Comp_misc.Source_tagger
module Target = Comp_misc.Target
module Ocaml_to_erlang = Ocaml_to_erlang

let tool_name = "caramel"

let default_stdlib_path =
  let ( / ) = Filename.concat in
  let dirname = Filename.dirname in
  let root = dirname (dirname Sys.executable_name) in
  root / "lib" / "caramel" / "stdlib"

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

let initialize_compiler ?(no_stdlib = false)
    ?(stdlib_path = default_stdlib_path) () =
  Clflags.nopervasives := true;
  Clflags.no_std_include := true;
  Clflags.open_modules := if no_stdlib then [] else [ "Beam" ];
  Clflags.include_dirs := if no_stdlib then [] else [ stdlib_path ];
  Compmisc.init_path ();
  let _ = Compmisc.initial_env () in
  ()

let compile_one source ~target ~opts =
  let open Source_tagger in
  let open Target in
  let fn, source_file =
    match (source, target) with
    | Mli file, _ -> (Optcompile.interface, file)
    | Ml file, Erlang -> (Ocaml_to_erlang.compile ~opts, file)
    | Ml file, Core_erlang -> (Ocaml_to_core_erlang.compile ~opts, file)
    | Rei file, _ -> (Reason_to_erlang.interface ~opts, file) (* TODO *)
    | Re file, Erlang -> (Reason_to_erlang.compile ~opts, file)
    | Re file, Core_erlang -> (Ocaml_to_core_erlang.compile ~opts, file)
    | Other (t, file, ext), _ ->
        raise (Unsupported_file_type_for_target (t, file, ext))
  in
  fn ~source_file ~output_prefix:(Filename.chop_extension source_file)

let compile ({ sources; targets; no_stdlib; stdlib_path; _ } as opts) =
  match
    initialize_compiler ~no_stdlib ~stdlib_path ();
    let target = List.hd targets in
    let sorted_sources = Source_tagger.prepare ~sources ~target in
    List.iter
      (fun target -> List.iter (compile_one ~target ~opts) sorted_sources)
      targets;
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
