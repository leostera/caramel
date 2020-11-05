open Compile_common

let tool_name = "caramelc:ocaml-archive"

let read_signature info =
  let module_name = info.module_name in
  let cmi_file = module_name ^ ".cmi" in
  try
    let intf_file = Load_path.find_uncap cmi_file in
    let sign = Env.read_signature module_name intf_file in
    Some sign
  with Not_found -> None

let archive ~source_file ~output_prefix ~opts:_ =
  let backend _info _typed =
    Bytelibrarian.create_archive
      (Compenv.get_objfiles ~with_ocamlparam:false)
      (output_prefix ^ ".cma")
  in
  Compile_common.with_info ~native:false ~tool_name ~source_file ~output_prefix
    ~dump_ext:"cma"
  @@ Compile_common.implementation ~backend
