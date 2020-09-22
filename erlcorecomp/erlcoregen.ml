open Compile_common

let generate_one erlmod =
  let open Erlcoreast in
  print_string ("Compiling " ^ erlmod.m_name ^ "\t");
  let erlfile = erlmod.m_filename in
  let oc = open_out_bin erlfile in
  Misc.try_finally
    ~always:(fun () ->
      print_string "OK\n";
      close_out oc)
    ~exceptionally:(fun () -> Misc.remove_file erlfile)
    (fun () ->
      let f = Format.formatter_of_out_channel oc in
      Format.fprintf f "%% Source code generated with Caramel.\n";
      Format.fprintf f "%a@\n%!" Printcore.pp erlmod)

let generate_sources file_info lambda =
  lambda |> Erlcoreconv.from_lambda ~name:file_info.module_name |> generate_one
