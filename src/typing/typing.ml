open Comp_misc
open Comp_misc.Opts

let initialize_compiler ~no_stdlib ~stdlib_path =
  Clflags.nopervasives := true;
  Clflags.no_std_include := true;
  Clflags.open_modules := if no_stdlib then [] else [ "Stdlib"; "Beam" ];
  Clflags.include_dirs :=
    if no_stdlib then []
    else
      [
        Filename.concat stdlib_path "ocaml"; Filename.concat stdlib_path "beam";
      ];
  Compmisc.init_path ();
  let _ = Compmisc.initial_env () in
  ()

let check_one source ~opts =
  let open Source_tagger in
  let fn, source_file =
    match source with
    | Mli file -> (Ocaml.interface, file)
    | Ml file -> (Ocaml.implementation, file)
    | Erl file -> (Erlang_as_ocaml.check ~dump_ast:opts.dump_ast, file)
    | Other (t, file, ext) ->
        raise (Unsupported_file_type_for_target (t, file, ext))
  in
  fn ~source_file ~output_prefix:(Filename.chop_extension source_file) |> ignore

let check ({ sources; targets; stdlib_path; no_stdlib; _ } as opts) =
  match
    initialize_compiler ~stdlib_path ~no_stdlib;
    let target = List.hd targets in
    Source_tagger.prepare ~sources ~target |> List.iter (check_one ~opts);
    Warnings.check_fatal ()
  with
  | exception Env.Error err ->
      Env.report_error Format.std_formatter err;
      Format.fprintf Format.std_formatter "\n%!"
  | exception exc ->
      Format.fprintf Format.std_formatter "ERROR: %s\n" (Printexc.to_string exc)
  | _ -> ()

let run_check f =
  try f () with
  | Env.Error err ->
      Env.report_error Format.std_formatter err;
      Format.fprintf Format.std_formatter "\n%!";
      exit 1
  | exc ->
      Format.fprintf Format.std_formatter "ERROR: %s\n" (Printexc.to_string exc);
      exit 1
