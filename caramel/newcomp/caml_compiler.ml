open Compile_common
open Typedtree

type t = {
  tool_name : string;
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

let compile_interface ~unit _t =
  Logs.debug (fun f ->
      f "Compiling interface unit: %a" Compilation_unit.pp unit);

  let source_file = Compilation_unit.source_file unit in
  (* compile the interface *)
  Optcompile.interface ~source_file
    ~output_prefix:(Filename.chop_extension source_file);
  Ok ()

let read_signature info =
  let module_name = info.module_name in
  let cmi_file = module_name ^ ".cmi" in
  try
    let intf_file = Load_path.find_uncap cmi_file in
    let sign = Env.read_signature module_name intf_file in
    Some sign
  with Not_found -> None

let compile_implementation ~unit handle_typedtree t =
  Logs.debug (fun f ->
      f "Compiling implementation unit: %a" Compilation_unit.pp unit);

  let source_file = Compilation_unit.source_file unit in
  let backend info (structure, _coercion) =
    let signature =
      match read_signature info with Some s -> s | None -> structure.str_type
    in
    let module_name =
      info.module_name |> Fpath.v |> Fpath.rem_ext ~multi:true |> Fpath.filename
    in
    Logs.debug (fun f ->
        f "Calling typedtree handler for unit: %a" Compilation_unit.pp unit);
    handle_typedtree ~unit ~module_name ~signature ~structure
  in
  Compile_common.with_info ~dump_ext:"cmo" ~native:false
    ~output_prefix:(Filename.chop_extension source_file)
    ~source_file ~tool_name:t.tool_name
    (Compile_common.implementation ~backend);
  Ok ()
