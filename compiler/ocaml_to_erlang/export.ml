module Erl = Erlang.Ast_helper
open Typedtree
open Types

(** Build the exports table of an Erlang module

    This will look for the signature of the module to determine what to export.

    The type-chain looks like this:
      * Typedtree.structure
      * Types.signature
      * types.signature_item
 *)
let mk_exports :
    Typedtree.structure -> Types.signature option -> Erlang.Ast.export list =
 fun typedtree signature ->
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
             Some (Erl.Export.fun_ name ~arity)
         | Sig_type (name, { type_arity = arity; _ }, _, Exported) ->
             let name = Names.atom_of_ident name in
             Some (Erl.Export.type_ name ~arity)
         | _ -> None)
