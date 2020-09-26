module Erl = Erlang.Ast_helper
open Typedtree
open Types

let rec is_unit (t : Types.type_expr) =
  match t.desc with
  | Tconstr (p, _, _) -> Path.same p Predef.path_unit
  | Tlink t' -> is_unit (Btype.repr t')
  | _ -> false

let is_opaque_in_signature type_decl signature =
  match signature with
  | None -> Erl.Type.visible
  | Some sign ->
      List.fold_left
        (fun visibility sig_item ->
          match sig_item with
          | Sig_type (name, { type_manifest = None; _ }, _, _)
            when Ident.name name = Ident.name type_decl.typ_id ->
              Erl.Type.opaque
          | _ -> visibility)
        Erl.Type.visible sign

let rec build_type_kind core_type =
  match core_type.ctyp_desc with
  | Ttyp_any -> Some Erl.Type.any
  | Ttyp_var var_name -> Some (Erl.Type.var (Erl.Name.var var_name))
  (* NOTE: OCaml works with functions from one type to another, and supports
   * multiargument functions via currying or tuples.
   *
   * Erlang doesn't, so we'll squash the function type into a single function
   * with multiple arguments instead.
   *)
  | Ttyp_arrow (_, param, out) ->
      let rec args t acc =
        match t.ctyp_desc with
        | Ttyp_arrow (_, p, t') -> args t' (p :: acc)
        | _ -> t :: acc |> List.rev
      in
      let args = args out [ param ] |> List.filter_map build_type_kind in
      let return = List.hd args in
      let args = List.rev (List.tl args) in
      Some (Erl.Type.fun_ ~args ~return)
  (* NOTE: this allows us to export type aliases that may have been made
   * opaque, such as `type opaque = string`, as `-type opaque() :: string().`
   *
   * It is also used for application of type constructors: `type a = string list`
   * gets compiled to `-type a() :: list(string()).`
   *)
  | Ttyp_constr (_, { txt; _ }, args) ->
      let name = Names.longident_to_type_name txt in
      let args = List.filter_map build_type_kind args in
      Some (Erl.Type.apply ~args ~name)
  | Ttyp_tuple els ->
      let parts = List.filter_map build_type_kind els in
      Some (Erl.Type.tuple parts)
  | Ttyp_variant (rows, _closed, _labels) ->
      let rec all_rows rs acc =
        match rs with
        | [] -> acc |> List.rev
        | r :: rs' -> (
            match r.rf_desc with
            | Ttag ({ txt; _ }, _, core_types) ->
                let name = Erl.Name.atom txt in
                let args = core_types |> List.filter_map build_type_kind in
                let variant = Erl.Type.constr ~name ~args in
                all_rows rs' (variant :: acc)
            | Tinherit { ctyp_desc = Ttyp_constr (_, { txt; _ }, args); _ } ->
                let name = Names.longident_to_type_name txt in
                let args = List.filter_map build_type_kind args in
                let t = Erl.Type.extension (Erl.Type.apply ~name ~args) in
                all_rows rs' (t :: acc)
            | _ -> all_rows rs' acc )
      in
      let constructors = all_rows rows [] in
      Some (Erl.Type.variant constructors)
  (* NOTE: these are two core type constructors that are essentially "links"
   * to follow.
   *
   * The second one `Ttyp_poly (strings, core_typ)` seemed to appear in records.
   *)
  | Ttyp_poly (_names, follow) -> build_type_kind follow
  | Ttyp_alias (follow, _) -> build_type_kind follow
  | Ttyp_object _ | Ttyp_class _ | Ttyp_package _ ->
      raise Error.Unsupported_feature

let build_record labels =
  let mk_field Typedtree.{ ld_id; ld_type; _ } =
    let rf_name = Names.atom_of_ident ld_id in
    let rf_type =
      match build_type_kind ld_type with Some t -> t | None -> Erl.Type.any
    in
    Erl.Type.field rf_name rf_type
  in
  let fields = List.map mk_field labels in
  Erl.Type.record fields

let build_abstract name params type_decl core_type signature =
  match build_type_kind core_type with
  | Some kind ->
      let visibility = is_opaque_in_signature type_decl signature in
      Some (Erl.Type.mk ~name ~params ~kind ~visibility)
  | None -> None

let build_type_params params =
  params
  |> List.filter_map (fun (core_type, _) ->
         match core_type.ctyp_desc with
         | Ttyp_var name -> Some (Erl.Name.var name)
         | _ -> None)

let build_type type_decl ~signature =
  let name = Names.atom_of_ident type_decl.typ_id in
  let params = build_type_params type_decl.typ_params in
  match type_decl.typ_kind with
  (* NOTE: turns out that "abstract" here means "only structure, no names!"
   * so this branch will generate the appropriate types for tuples, aliases
   * and actual abstract types.
   * *)
  | Ttype_abstract -> (
      match type_decl.typ_manifest with
      | Some abs -> build_abstract name params type_decl abs signature
      | None ->
          let kind =
            Erl.Type.apply ~args:[] ~name:(Erl.Name.atom "reference")
          in
          Some (Erl.Type.mk ~name ~params ~kind ~visibility:Opaque) )
  | Ttype_record labels ->
      let kind = build_record labels in
      Some (Erl.Type.mk ~name ~params ~kind ~visibility:Visible)
  | Ttype_variant constructors ->
      let mk_constr Typedtree.{ cd_id; cd_args; _ } =
        let args =
          match cd_args with
          | Cstr_tuple core_types ->
              core_types |> List.filter_map build_type_kind
          | Cstr_record labels -> [ build_record labels ]
        in
        Erl.Type.constr ~name:(Names.name_of_ident cd_id) ~args
      in
      let constructors = List.map mk_constr constructors in
      Some
        (Erl.Type.mk ~name ~params
           ~kind:(Erl.Type.variant constructors)
           ~visibility:Visible)
  | Ttype_open ->
      Some (Erl.Type.mk ~name ~params ~kind:Erl.Type.any ~visibility:Visible)

(** Build the types of an Erlang module.
 *)
let build_types :
    Typedtree.structure -> Types.signature option -> Erlang.Ast.type_decl list =
 fun typedtree signature ->
  typedtree.str_items
  |> List.concat_map (fun item ->
         match item.str_desc with Tstr_type (_, tys) -> tys | _ -> [])
  |> List.filter_map (build_type ~signature)
