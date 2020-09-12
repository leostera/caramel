open Compile_common

let generate_one erlmod =
  let open Erlast in
  let erlfile = erlmod.file_name in
  let oc = open_out_bin erlfile in
  Misc.try_finally
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> Misc.remove_file erlfile)
    (fun () ->
        let f = Format.formatter_of_out_channel oc in
        Format.fprintf f "%% Source code generated with Caramel.\n";
        Format.fprintf f "%a@\n" Printerl.pp erlmod;
        Format.fprintf f "%!";
    )

let generate_sources file_info (typedtree, _mod_coercion) signature =
  let erlmods = Erlconv.from_typedtree ~name:file_info.module_name typedtree signature in
  let _ = print_string ("Generating " ^ (string_of_int (List.length erlmods)) ^ " sources...\n" ) in
  List.iter generate_one erlmods
