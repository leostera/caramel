open Typedtree
open Types

(** Build the exports table of an Erlang module

    This will look for the signature of the module to determine what to export.

    The type-chain looks like this:
      * Typedtree.structure
      * Types.signature
      * types.signature_item
 *)
let build_exports :
    name:string ->
    Typedtree.structure ->
    Types.signature option ->
    Erlang.Ast.export list =
 fun ~name:_ typedtree signature ->
  let rec collect_args value args =
    match value.desc with
    | Tarrow (_, arg, next, _) -> collect_args next (arg :: args)
    | Tlink t -> collect_args (Btype.repr t) args
    | _ -> args
  in

  let signature =
    match signature with None -> typedtree.str_type | Some x -> x
  in

  signature
  |> List.filter_map (fun sig_item ->
         match sig_item with
         | Sig_value (_, { val_kind = Val_prim _; _ }, Exported) -> None
         | Sig_value (name, vd, Exported) ->
             let name = Names.atom_of_ident name in
             let args =
               match collect_args vd.val_type [] with
               | t :: rest when Typespecs.is_unit t -> rest
               | args -> args
             in
             let arity = List.length args in
             Some (Erlang.Ast.make_fn_export name arity)
         | Sig_type (name, td, _, Exported) ->
             Some
               (Erlang.Ast.make_type_export (Names.atom_of_ident name) td.type_arity)
         | _ -> None)
