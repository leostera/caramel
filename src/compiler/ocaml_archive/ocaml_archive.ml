let tool_name = "caramelc:ocaml-archive"

let archive ~source_file ~output_prefix ~opts:_ =
  let backend _info _typed =
    Bytelibrarian.create_archive
      (Compenv.get_objfiles ~with_ocamlparam:false)
      (output_prefix ^ ".cma")
  in
  Compile_common.with_info ~native:false ~tool_name ~source_file ~output_prefix
    ~dump_ext:"cma"
  @@ Compile_common.implementation ~backend
