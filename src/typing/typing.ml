open Comp_misc
open Comp_misc.Opts

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

let check_one source ~opts =
  let open Source_tagger in
  let fn, source_file =
    match source with
    | Mli file -> (Optcompile.interface, file)
    | Ml file -> (Ocaml_archive.archive ~opts, file)
    | Erl file -> (Erlang_as_ocaml.check ~opts, file)
    | Other (t, file, ext) ->
        raise (Unsupported_file_type_for_target (t, file, ext))
  in
  fn ~source_file ~output_prefix:(Filename.chop_extension source_file)

let check ({ sources; targets; _ } as opts) =
  match
    initialize_compiler ~opts;
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
