open Typedtree
open Types

let normalize_name = String.lowercase_ascii
let name_of_ident i = i |> Ident.name |> normalize_name

(** Build a single Erlang module from a Typedtree.structure.

    This will look for the signature of the module to determine what to export.

    The type-chain looks like this for nested modules:
      * Typedtree.structure
      * Typedtree.structure_item
      * Typedtree.structure_item_desc
      * Typedtree.module_binding
      * Typedtree.module_expr
      * Typedtree.module_type
      * Typedtree.signature
      * Typedtree.signature_item

    but we are not following those!

 *)
let build_module: name:string -> Typedtree.structure -> Erlast.t =
  fun ~name typedtree ->
    let rec value_arity = fun value count ->
      match value.desc with
      | Tarrow (_, _, next, _) -> value_arity next (count + 1)
      | Tlink t -> value_arity t count
      | _ -> count
    in

    let exports = typedtree.str_type |> (List.filter_map (fun sig_item ->
      match sig_item  with
      | Sig_value (name, { val_type  }, _) ->
          Some (name_of_ident name, value_arity val_type 0)
      | Sig_type (name, { type_arity }, _, Exported) ->
          Some (name_of_ident name, type_arity)
      | _  -> None
    )) |> List.map Erlast.make_export in

    Erlast.make ~name ~exports

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
let rec find_modules: prefix:string -> Typedtree.structure -> (string * Typedtree.structure) list =
  fun ~prefix typedtree ->
    let module_name prefix mb_id = (match mb_id with
          | Some x -> prefix ^ "_" ^ (name_of_ident x)
          | None -> prefix) |> String.lowercase_ascii
    in
    typedtree.str_items
    |> (List.fold_left (fun acc struct_item ->
        let mbs = (match struct_item.str_desc with
            | Tstr_module mb -> [mb]
            | Tstr_recmodule mbs ->  mbs
            | _ -> [])
            |> (List.concat_map (fun mb ->
                let prefix = module_name prefix mb.mb_id in
                match mb.mb_expr.mod_desc with
                  | Tmod_structure typedtree -> (prefix, typedtree) :: (find_modules ~prefix typedtree)
                  | _ -> []
                )) in
        List.concat [mbs; acc]
    ) [])


(** Turn an OCaml Typedtree into a list of Erlang ASTs that can be compiled to
    sources.
*)
let from_typedtree: name:string -> Typedtree.structure -> Erlast.t list =
  fun ~name typedtree ->
    let name = normalize_name name in
    [

      (find_modules ~prefix:name typedtree)
      |> List.map( fun (name, mb) -> build_module ~name mb );

      [ build_module ~name typedtree ];

    ] |> List.concat
