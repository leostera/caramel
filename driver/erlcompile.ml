(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc
open Compile_common

let tool_name = "caramelc"

let with_info =
  Compile_common.with_info ~native:false ~tool_name

let interface ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~dump_ext:"cmi" @@ fun info ->
  Compile_common.interface info

(** Bytecode compilation backend for .ml files. *)

let to_bytecode i lambda =
  lambda
  |> Profile.(record ~accumulate:true generate)
    (fun { Lambda.code = lambda; required_globals } ->
       lambda
       |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
       |> Simplif.simplify_lambda
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
       |> Bytegen.compile_implementation i.module_name
       |> print_if i.ppf_dump Clflags.dump_instr Printinstr.instrlist
       |> fun bytecode -> bytecode, required_globals
    )

let to_lambda i (typedtree, coercion) =
  (typedtree, coercion)
  |> Profile.(record transl) (Translmod.transl_implementation i.module_name)

let emit_bytecode i (bytecode, required_globals) =
  let cmofile = cmo i in
  let oc = open_out_bin cmofile in
  Misc.try_finally
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> Misc.remove_file cmofile)
    (fun () ->
       bytecode
       |> Profile.(record ~accumulate:true generate)
         (Emitcode.to_file oc i.module_name cmofile ~required_globals);
    )

let read_signature info =
  let module_name = info.module_name in
  let cmi_file = module_name ^ ".cmi" in
  try begin
    let intf_file = Load_path.find_uncap cmi_file in
    let sign = Env.read_signature module_name intf_file  in
    Some sign
  end
    with Not_found -> None

(*
 * Entrypoint to generate Erlang from OCaml
 *)
let ml_to_erlang ~source_file ~output_prefix =
  let backend info typed =
    let lambda = to_lambda info typed in
    let bytecode = to_bytecode info lambda in
    let _ = emit_bytecode info bytecode in
    let signature = read_signature info in
    Erlgen.generate_sources info typed signature
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmo" @@ fun info ->
    Compile_common.implementation info ~backend

(*
 * Entrypoint to generate Core Erlang from OCaml
 *)
let ml_to_core_erlang ~source_file ~output_prefix =
  let backend info typed =
    let lambda = to_lambda info typed in
    let bytecode = to_bytecode info lambda in
    let _ = emit_bytecode info bytecode in
    Erlcoregen.generate_sources info lambda.code
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmo" @@ fun info ->
    Compile_common.implementation info ~backend

(*
 * Entrypoint to typecheck Erlang
 *)
let erl_to_cmi ~source_file ~output_prefix =
  let backend info typed =
    let lambda = to_lambda info typed in
    let bytecode = to_bytecode info lambda in
    emit_bytecode info bytecode
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmo" @@ (fun i ->
      let ic = open_in_bin i.source_file in
      let lexbuf = Lexing.from_channel ic in
      let parsetree = Erlparser.implementation Erllexer.token lexbuf in
      let typedtree = parsetree
      |> Profile.(record typing)
         (Typemod.type_implementation
            i.source_file i.output_prefix i.module_name i.env)
      |> print_if i.ppf_dump Clflags.dump_typedtree Printtyped.implementation_with_coercion in
      backend i typedtree;
  )
