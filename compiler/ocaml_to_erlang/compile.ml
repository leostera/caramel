module Erl = Erlang.Ast_helper
open Typedtree
open Types

(** Build a single Erlang module from a Typedtree.structure, and an optionally
    constraining Types.signature.
 *)
let build_module :
    module_name:Erlang.Ast.atom ->
    modules:Erlang.Ast.t list ->
    Typedtree.structure ->
    Types.signature option ->
    Erlang.Ast.t =
 fun ~module_name ~modules typedtree signature ->
  let exports = Export.build_exports typedtree signature in
  let types = Typespecs.build_types typedtree signature in
  let functions = Fun.build_functions ~module_name ~modules typedtree in
  let attributes = [] in
  let behaviours = [] in
  Erl.Mod.mk ~behaviours ~exports ~types ~functions ~attributes module_name

(** Navigate a [Typedtree.structure] and recursively collect all module definitions,
    building up the right prefixed names.

    The type-chain looks like this:
      * Typedtree.structure
      * Typedtree.structure_item
      * Typedtree.structure_item_desc
      * Typedtree.module_binding
      * Typedtree.module_expr
      * Typedtree.module_expr_desc
      * Typedtree.structure -> back to the top again
 *)
let rec find_modules :
    prefix:Erlang.Ast.atom ->
    Typedtree.structure ->
    (Erlang.Ast.atom * Typedtree.structure * Types.signature option) list =
 fun ~prefix typedtree ->
  typedtree.str_items
  |> List.fold_left
       (fun acc struct_item ->
         let mbs =
           ( match struct_item.str_desc with
           | Tstr_module mb -> [ mb ]
           | Tstr_recmodule mbs -> mbs
           | _ -> [] )
           |> List.concat_map (fun mb ->
                  let ocaml_name =
                    match mb.mb_id with Some x -> Ident.name x | None -> ""
                  in
                  let module_name =
                    Erl.Atom.concat prefix (Erl.Atom.mk ocaml_name)
                  in
                  match mb.mb_expr.mod_desc with
                  | Tmod_constraint
                      ( { mod_desc = Tmod_structure typedtree; _ },
                        Mty_signature signature,
                        _mod_type_constr,
                        _mod_type_coerc ) ->
                      (module_name, typedtree, Some signature)
                      :: find_modules ~prefix typedtree
                  | Tmod_structure typedtree ->
                      (module_name, typedtree, None)
                      :: find_modules ~prefix typedtree
                  | _ -> [])
         in
         List.concat [ mbs; acc ])
       []

(** Turn an OCaml Typedtree into a list of Erlang ASTs that can be compiled to
    sources.
*)
let from_typedtree :
    module_name:string ->
    Typedtree.structure ->
    Types.signature option ->
    Erlang.Ast.t list =
 fun ~module_name typedtree signature ->
  let top_module = Erl.Atom.mk module_name in
  let modules =
    List.fold_left
      (fun mods (nested_module_name, impl, sign) ->
        build_module ~module_name:nested_module_name ~modules:mods impl sign
        :: mods)
      []
      (find_modules ~prefix:top_module typedtree)
  in
  [
    modules;
    [ build_module ~module_name:top_module ~modules typedtree signature ];
  ]
  |> List.concat
