open Compile_common

exception Cannot_compile_file

type compilation = { sources : string list }

let to_bytecode i lambda =
  lambda
  |> Profile.(record ~accumulate:true generate)
       (fun Lambda.{ code = lambda; required_globals; _ } ->
         let simp_lambda = lambda |> Simplif.simplify_lambda in
         let bytecode =
           simp_lambda |> Bytegen.compile_implementation i.module_name
         in
         (bytecode, required_globals))

let to_lambda i (typedtree, coercion) =
  (typedtree, coercion)
  |> Profile.(record transl) (Translmod.transl_implementation i.module_name)

let emit_bytecode i (bytecode, required_globals) =
  let cmofile = i.output_prefix ^ ".cmo" in
  let oc = open_out_bin cmofile in
  Misc.try_finally
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> Misc.remove_file cmofile)
    (fun () ->
      bytecode
      |> Profile.(record ~accumulate:true generate)
           (Emitcode.to_file oc i.module_name cmofile ~required_globals))

let read_signature info =
  let module_name = info.module_name in
  let cmi_file = module_name ^ ".cmi" in
  try
    let intf_file = Load_path.find_uncap cmi_file in
    let sign = Env.read_signature module_name intf_file in
    Some sign
  with Not_found -> None

let initialize_compiler () =
  Compmisc.init_path ();
  let _ = Compmisc.initial_env () in
  ()

(** Actual compilation chains *)

let ml_to_erlang ~source_file ~output_prefix =
  let backend info (typed, coercion) =
    let lambda = to_lambda info (typed, coercion) in
    let bytecode = to_bytecode info lambda in
    let _ = emit_bytecode info bytecode in
    let signature = read_signature info in
    Ocaml_to_erlang.from_typedtree ~name:info.module_name typed signature
    |> Erlang.Printer.to_sources
  in
  Compile_common.with_info ~native:false ~tool_name:"caramelc" ~source_file
    ~output_prefix ~dump_ext:"cmo"
  @@ fun info -> Compile_common.implementation info ~backend

let mli_to_erlang ~source_file ~output_prefix =
  Compile_common.with_info ~native:false ~tool_name:"caramelc" ~source_file
    ~output_prefix ~dump_ext:"cmi"
  @@ Compile_common.interface

(* Entrypoint to typecheck Erlang *)
let erl_to_cmi ~source_file ~output_prefix =
  let backend info typed =
    let lambda = to_lambda info typed in
    let bytecode = to_bytecode info lambda in
    emit_bytecode info bytecode
  in
  Compile_common.with_info ~native:false ~tool_name:"caramelc" ~source_file
    ~output_prefix ~dump_ext:"cmo"
  @@ fun i ->
  let ic = open_in_bin i.source_file in
  let lexbuf = Lexing.from_channel ic in
  let parsetree = Erlang.Parser.implementation Erlang.Lexer.token lexbuf in
  let typedtree =
    parsetree
    |> Profile.(record typing)
         (Typemod.type_implementation i.source_file i.output_prefix
            i.module_name i.env)
  in
  backend i typedtree

let compile_one source =
  let fn, source_file =
    match source with
    | `Ml file -> (ml_to_erlang, file)
    | `Mli file -> (mli_to_erlang, file)
    | `Erl file -> (erl_to_cmi, file)
    | `Unsupported_file_type _ -> raise Cannot_compile_file
  in
  fn ~source_file ~output_prefix:(Filename.chop_extension source_file)

let tag_source filename =
  match Filename.extension filename with
  | ".ml" -> `Ml filename
  | ".mli" -> `Mli filename
  | ".erl" -> `Erl filename
  | ext -> `Unsupported_file_type (filename, ext)

let compile { sources; _ } =
  match
    initialize_compiler ();

    let tagged_sources, errs =
      sources |> List.map tag_source
      |> List.partition (function `Unsupported_file _ -> false | _ -> true)
    in

    errs
    |> List.iter (function
         | `Unsupported_file_type (file, ext) ->
             Format.fprintf Format.std_formatter
               "ERROR: Filename %s has unsupported extension %s." file ext;
             exit 1
         | _ -> ());

    let ml_sources =
      tagged_sources
      |> List.filter_map (function `Ml f | `Mli f -> Some f | _ -> None)
      |> Dependency_sorter.sorted_files |> List.map tag_source
    in

    let erlang_sources =
      tagged_sources
      |> List.filter_map (function `Erl f -> Some (`Erl f) | _ -> None)
    in

    List.iter compile_one (ml_sources @ erlang_sources)
  with
  | exception Env.Error err -> Env.report_error Format.std_formatter err
  | exception exc ->
      Format.fprintf Format.std_formatter "ERROR: %s" (Printexc.to_string exc)
  | _ -> ()
