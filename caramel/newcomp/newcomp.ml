module Source_kind = struct
  type t =
    | Interface
    | Implementation 
end

module Compilation_unit = struct
  type t = {
    source_kind: Source_kind.t ;
    source_file: Fpath.v ;
    target_file: Fpath.v ;
  }

  let compile t =
end

module Caramel_compiler = struct

  let compile ~unit ~typedtree ~module_name ~signature =
    let ast = Ast_transl.from_typedtree ~module_name ~signature typedtree in
    Erlang.Printer.to_sources erl_ast

end


module Caml_compiler = struct
  open Compile_common

  type cfg = {
    stdlib: Fpath.v option
  }

  type t = { cfg: cfg }

  let from_config cfg = { cfg=cfg }

  let init {cfg = {stdlib}; _} =
    let no_stdlib = (stdlib = None) in
    Clflags.nopervasives := true;
    Clflags.no_std_include := true;
    Clflags.open_modules := if no_stdlib then [] else [ "Beam" ];
    Clflags.include_dirs := if no_stdlib then [] else [ stdlib_path ];
    Compmisc.init_path ();
    let _ = Compmisc.initial_env () in
    ()

  let compile_interface ~unit =
    (* compile the interface *)
    Optcompile.interface
      ~source_file:unit.source_file
      ~output_prefix:(Filename.chop_extension unit.source_file)
    (* TODO: move the file on unit.target_file *)

  let read_signature info =
    let module_name = info.module_name in
    let cmi_file = module_name ^ ".cmi" in
    try
      let intf_file = Load_path.find_uncap cmi_file in
      let sign = Env.read_signature module_name intf_file in
      Some sign
    with Not_found -> None

  let compile_implementation ~unit =
    let backend info (typed, _coercion) =
      let signature = read_signature info in
      let module_name = info.module_name in
      Caramel_compiler.compile ~unit ~typed_tree ~module_name ~signature
    in
    Compile_common.with_info ~native:false ~tool_name ~source_file ~output_prefix
      ~dump_ext:"cmo"
    @@ fun info -> Compile_common.implementation info ~backend


  let compile ~unit =
    match unit.source_kind with
    | Interface -> compile_interface ~unit
    | Implementation -> compile_implementation ~unit


end

module Builder = struct
  type t = {
    units: Compilation_unit.t list
  }

  let make () = { units: [] }

  let add t ~sources = {t with units = t.units @@ sources }

end

module Runner = struct
  module Options = struct
    type t = {
      sources: Fpath.t list;
    }
  end

  type t = {
    options: Options.t;
  }

  let from_opts options = { options=options }

  let run { options; _ } =
    let caml = Caml_compiler.from_config {
      stdlib = if options.no_stdlib then None else Some options.stdlib_path) 
    } in
    let builder = Builder.make () |> Builder.add ~sources in
    Caml_compiler.init caml

end
