module Attribute = struct
  open Types

  let exports ~signature =
    List.filter_map
      (function
        (* NOTE(@ostera): what did this do again? *)
        | Sig_value (_, { val_kind = Val_prim _; _ }, Exported) -> None
        (* Handle exported functions *)
        | Sig_value (ident, value_desc, Exported) ->
            Some (Function.export_from_signature ~ident ~value_desc)
        (* Handle exported types *)
        | Sig_type (ident, value_desc, _, Exported) ->
            Some (Type.export_from_signature ~ident ~value_desc)
        (* Hide everything else *)
        | _ -> None)
      signature

  let from_ocaml ~structure:_ ~signature = List.concat [ exports ~signature ]
end

(* TODO: remove! we're not handling the module system anymore so module
   inclusion won't be a thing. *)
let rec flatten_module_inclusion ~ctx transl_input fn =
  let open Typedtree in
  let open Translation_input in
  List.concat_map
    (fun str_item ->
      match str_item.str_desc with
      | Tstr_include { incl_mod = { mod_desc; _ }; _ } -> (
          match
            Translation_input.of_mod_expr_desc mod_desc
              ~filename:transl_input.filename
              ~caml_module_path:transl_input.caml_module_path
          with
          | Some input -> fn input @ flatten_module_inclusion ~ctx input fn
          | None -> [])
      | _ -> [])
    transl_input.caml_structure.str_items

(**
   Build a single Erlang module from a Typedtree.structure, and an optionally
   constraining Types.signature.

   It will take care of inlining all module-level constructs.
 *)
let make ~ctx:_ _ =
  (* TODO: create an Ir.mod_desc here
     (* TODO: this should be a name path (string list) *)
     let module_name =
       Identifier.Module_name.from_caml_module_path caml_module_path
     in

     Logs.debug (fun f ->
         f "Creating module: %s" (Identifier.Module_name.to_string module_name));

     (* TODO: move elsewhere? we're not handling the module system anymore. *)
     let inlined_funs =
       flatten_module_inclusion ~ctx transl_input (Function.from_ocaml ~ctx)
     in

     Logs.debug (fun f ->
         f "Found %d inlined functions from included modules"
           (List.length inlined_funs));

        Erl.Mod.mk ~ctx:Erl.Loc.empty ~mod_ctx:transl_input
          ~attributes:
            (Attribute.from_ocaml ~structure:caml_structure ~signature:caml_signature)
          ~behaviours:[]
          ~functions:(inlined_funs @ Function.from_ocaml ~ctx transl_input)
          ~file_name:(Identifier.Module_name.to_file_name module_name)
          ~module_name:(Identifier.Module_name.to_atom module_name)
  *)
  Error.todo "Module.make"
