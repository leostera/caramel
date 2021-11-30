open Erlang.Parsetree

let write_file pp file_name impl =
  Logs.debug (fun f -> f "Writing %s\t" file_name);
  let oc = open_out_bin file_name in
  (try
     let ppf = Format.formatter_of_out_channel oc in
     Format.fprintf ppf "%a" pp impl
   with _ -> Sys.remove file_name);
  close_out oc;
  Logs.debug (fun f -> f "OK\n")

let write_sources outs =
  List.iter
    (fun impl -> write_file Erlang.Parsetree_printer.Mod.pp impl.file_name impl)
    outs

let write_asts outs =
  let pp_sexp ppf impl =
    let sexp =
      Erlang.Parsetree.sexp_of_implementation
        (fun _ -> Sexplib.Sexp.List [])
        (fun _ -> Sexplib.Sexp.List [])
        impl
    in
    let str = Sexplib.Sexp.to_string_hum ~indent:2 sexp in
    Format.fprintf ppf "%s\n\n%!" str
  in
  List.iter (fun impl -> write_file pp_sexp (impl.file_name ^ ".ast") impl) outs

let write_files ~kind impls =
  match kind with `ast -> write_asts impls | `sources -> write_sources impls
