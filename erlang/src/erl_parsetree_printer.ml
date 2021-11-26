open Erl_parsetree

module Atom = struct
  let pp ppf (Atom (_ctx, atom)) = Format.fprintf ppf "%s" atom
end

module Mod = struct
  let pp ppf { module_name; _ } =
    Format.fprintf ppf "-module(%a).\n" Atom.pp module_name

  let to_source_file m =
    let _ = print_string ("Compiling " ^ m.file_name ^ "\t") in
    let oc = open_out_bin m.file_name in
    (try
       let f = Format.formatter_of_out_channel oc in
       Format.fprintf f "%% Source code generated with Caramel.\n";
       Format.fprintf f "%a@\n%!" pp m
     with _ -> Sys.remove m.file_name);
    print_string "OK\n";
    close_out oc
end

let to_source_files mods = List.iter Mod.to_source_file mods
