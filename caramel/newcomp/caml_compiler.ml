open Compile_common

type t = {
  tool_name : string;
  output_prefix : string;
  stdlib : Fpath.t option;
  opened_modules : string list;
  include_dirs : Fpath.t list;
}

let init ({ stdlib; opened_modules; include_dirs; _ } as t) =
  let include_dirs = List.map Fpath.to_string include_dirs in
  (*
    Do not automatically open any modules that belong to the OCaml
    core libraries.

    However, do open modules we're told to!
  *)
  Clflags.nopervasives := true;
  Clflags.no_std_include := true;
  Clflags.open_modules := opened_modules;
  (*
    If we're given a standard library, make sure to include it as part
    of the include directories.
  *)
  let stdlib =
    match stdlib with None -> [] | Some p -> [ Fpath.to_string p ]
  in
  Clflags.include_dirs := include_dirs @ stdlib;
  Compmisc.init_path ();
  let _env = Compmisc.initial_env () in
  t

let compile_interface ~unit =
  (* compile the interface *)
  Optcompile.interface ~source_file:unit.source_file
    ~output_prefix:(Filename.chop_extension unit.source_file)

let read_signature info =
  let module_name = info.module_name in
  let cmi_file = module_name ^ ".cmi" in
  try
    let intf_file = Load_path.find_uncap cmi_file in
    let sign = Env.read_signature module_name intf_file in
    Some sign
  with Not_found -> None

let compile_implementation ~unit ~handle_typedtree t =
  let backend info (typedtree, _coercion) =
    let signature = read_signature info in
    let module_name = info.module_name in
    handle_typedtree ~module_name ~signature typedtree
  in
  Compile_common.with_info ~dump_ext:"cmo" ~native:false
    ~output_prefix:t.output_prefix ~source_file:unit.source_file
    ~tool_name:t.tool_name
    (Compile_common.implementation ~backend)
