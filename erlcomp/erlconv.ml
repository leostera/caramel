open Typedtree
open Types

let normalize_name = String.lowercase_ascii
let name_of_ident i = i |> Ident.name |> normalize_name

(** Build the exports table of an Erlang module

    This will look for the signature of the module to determine what to export.

    The type-chain looks like this:
      * Typedtree.structure
      * Types.signature
      * types.signature_item
 *)
let build_exports: name:string -> Typedtree.structure -> Types.signature option -> Erlast.export list =
  fun ~name typedtree signature ->
    let rec value_arity = fun value count ->
      match value.desc with
      | Tarrow (_, _, next, _) -> value_arity next (count + 1)
      | Tlink t -> value_arity t count
      | _ -> count
    in

    let signature = match signature with
      | None -> typedtree.str_type
      | Some x -> x
    in

    Format.fprintf Format.std_formatter "%s: \n" name;
    Printtyp.printed_signature "" Format.std_formatter signature;
    Format.fprintf Format.std_formatter "\n\n";

    signature |> (List.filter_map (fun sig_item ->
      match sig_item  with
      | Sig_value (name, { val_type }, Exported) ->
          Some (name_of_ident name, value_arity val_type 0)
      | Sig_type (name, { type_arity }, _, Exported) ->
          Some (name_of_ident name, type_arity)
      | _  -> None
    )) |> List.map Erlast.make_export

(** Build a single Erlang module from a Typedtree.structure, and an optionally
    constraining Types.signature.
 *)
let build_module: name:string -> Typedtree.structure -> Types.signature option -> Erlast.t =
  fun ~name typedtree signature ->
    let exports = build_exports ~name typedtree signature in
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
let rec find_modules:
  prefix:string
  -> Typedtree.structure
  -> (string * Typedtree.structure * (Types.signature option)) list =
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
                  | Tmod_constraint ({ mod_desc = Tmod_structure typedtree },
                                     Mty_signature signature,
                                     _mod_type_constr,
                                     _mod_type_coerc) ->
                      (prefix, typedtree, Some signature) :: (find_modules ~prefix typedtree)
                  | Tmod_structure typedtree -> (prefix, typedtree, None) :: (find_modules ~prefix typedtree)
                  | _ -> []
                )) in
        List.concat [mbs; acc]
    ) [])


(** Turn an OCaml Typedtree into a list of Erlang ASTs that can be compiled to
    sources.
*)
let from_typedtree:
  name:string
  -> Typedtree.structure
  -> (Types.signature option)
  -> Erlast.t list =
  fun ~name typedtree signature ->
    let name = normalize_name name in
    [
      (find_modules ~prefix:name typedtree)
      |> List.map( fun (name, impl, sign) -> build_module ~name impl sign );

      [ build_module ~name typedtree signature ];
    ] |> List.concat
