module Erl = Erlang.Parsetree_helper

module Attribute = struct
  open Types

  let exports ~signature =
    List.filter_map
      (function
        (* NOTE(@ostera): what did this do again? *)
        | Sig_value (_, { val_kind = Val_prim _; _ }, Exported) -> None
        (* Handle exported functions *)
        | Sig_value (name, value_desc, Exported) ->
            Some (Function.export_from_signature ~name ~value_desc)
        (* Handle exported types *)
        | Sig_type (name, value_desc, _, Exported) ->
            Some (Type.export_from_signature ~name ~value_desc)
        (* Hide everything else *)
        | _ -> None)
      signature

  let from_ocaml ~structure:_ ~signature = List.concat [ exports ~signature ]
end

type t = {
  module_name : Identifier.Module_name.t;
  structure : Typedtree.structure;
  signature : Types.signature;
}

(**
   Build a single Erlang module from a Typedtree.structure, and an optionally
   constraining Types.signature.

   It will take care of inlining all module-level constructs.
 *)
let make ({ module_name; structure; signature } as mod_ctx) =
  Logs.debug (fun f ->
      f "Creating module: %s" (Identifier.Module_name.to_string module_name));

  (* NOTE: where to put these? *)
  let inlined_funs =
    let rec inline_declarations str =
      let open Typedtree in
      List.concat_map
        (fun str_item ->
          match str_item.str_desc with
          | Tstr_include
              { incl_mod = { mod_desc = Tmod_structure structure; _ }; _ } ->
              Function.from_ocaml ~structure
              @ inline_declarations structure.str_items
          | _ -> [])
        str
    in
    inline_declarations structure.str_items
  in

  Erl.Mod.mk ~ctx:Erl.Loc.empty ~mod_ctx
    ~attributes:(Attribute.from_ocaml ~structure ~signature)
    ~behaviours:[]
    ~functions:(inlined_funs @ Function.from_ocaml ~structure)
    ~file_name:(Identifier.Module_name.to_file_name module_name)
    ~module_name:(Identifier.Module_name.to_atom module_name)

(**
   Build an Erlang module from an OCaml module descriptor.

   If the module is constrained by a signature, make sure we keep it around for
   later.
  *)
let from_mod_desc ~module_name ~mod_desc =
  let open Typedtree in
  let open Types in
  match mod_desc with
  | Tmod_constraint
      ( { mod_desc = Tmod_structure structure; _ },
        Mty_signature signature,
        _mod_type_constr,
        _mod_type_coerc ) ->
      Some (make { module_name; structure; signature })
  | Tmod_structure structure ->
      Some (make { module_name; structure; signature = structure.str_type })
  | _ -> None

(**
   The tree visitor knows how to traverse an OCaml Typedtree.structure,
   which represent the implementation files, and return a list of Erlang
   modules.
  *)
module Tree_visitor = struct
  open Typedtree

  let extract_modules str_desc =
    match str_desc with
    | Tstr_module mb -> [ mb ]
    | Tstr_recmodule mbs -> mbs
    | _ -> []

  let rec find_modules ~prefix ~structure =
    List.fold_left
      (fun acc struct_item ->
        List.concat_map
          (fun mb ->
            let ident = mb.mb_id in
            let module_name =
              Identifier.Module_name.from_ocaml ~prefix ~ident
            in
            let mod_desc = mb.mb_expr.mod_desc in
            match from_mod_desc ~module_name ~mod_desc with
            | Some m ->
                let structure = m.mod_ctx.structure in
                m :: find_modules ~prefix:module_name ~structure
            | None -> [])
          (extract_modules struct_item.str_desc)
        @ acc)
      [] structure.str_items
end
