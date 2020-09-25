open Typedtree
open Types

(** Build a single Erlang module from a Typedtree.structure, and an optionally
    constraining Types.signature.
 *)
let build_module :
    name:string ->
    ocaml_name:string ->
    modules:Erlang.Ast.t list ->
    Typedtree.structure ->
    Types.signature option ->
    Erlang.Ast.t =
 fun ~name ~ocaml_name ~modules typedtree signature ->
  let exports = Export.build_exports ~name typedtree signature in
  let types = Typespecs.build_types typedtree signature in
  let functions = Fun.build_functions ~module_name:name ~modules typedtree in
  let attributes = [] in
  Erlang.Ast.make ~name ~ocaml_name ~exports ~types ~functions ~attributes

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
    prefix:string ->
    Typedtree.structure ->
    (string * string * Typedtree.structure * Types.signature option) list =
 fun ~prefix typedtree ->
  let module_name prefix mb_id =
    ( match mb_id with
    | Some x -> prefix ^ "__" ^ Names.atom_of_ident x
    | None -> prefix )
    |> String.lowercase_ascii
  in
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
                  let prefix = module_name prefix mb.mb_id in
                  match mb.mb_expr.mod_desc with
                  | Tmod_constraint
                      ( { mod_desc = Tmod_structure typedtree; _ },
                        Mty_signature signature,
                        _mod_type_constr,
                        _mod_type_coerc ) ->
                      (prefix, ocaml_name, typedtree, Some signature)
                      :: find_modules ~prefix typedtree
                  | Tmod_structure typedtree ->
                      (prefix, ocaml_name, typedtree, None)
                      :: find_modules ~prefix typedtree
                  | _ -> [])
         in
         List.concat [ mbs; acc ])
       []

(** Turn an OCaml Typedtree into a list of Erlang ASTs that can be compiled to
    sources.
*)
let from_typedtree :
    name:string ->
    Typedtree.structure ->
    Types.signature option ->
    Erlang.Ast.t list =
 fun ~name typedtree signature ->
  let name = Names.atom_of_string name in
  let modules =
    List.fold_left
      (fun mods (name, ocaml_name, impl, sign) ->
        build_module ~name ~ocaml_name ~modules:mods impl sign :: mods)
      []
      (find_modules ~prefix:name typedtree)
  in
  [
    modules;
    [ build_module ~name ~ocaml_name:name ~modules typedtree signature ];
  ]
  |> List.concat
