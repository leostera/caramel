open Lambda

module Erlang = struct
  let m = "erlang"

  let not_equal = (m, "=/=")

  let less_than = (m, "<")

  let greater_than = (m, ">")

  let greater_or_equal = (m, ">=")

  let less_or_equal = (m, "=<")

  let equal = (m, "=:=")

  let is_integer = (m, "is_integer")

  let throw = (m, "throw")

  let hd = (m, "hd")

  let tl = (m, "tl")

  let element = (m, "element")
end

module Caramel = struct
  module Core = struct
    module Prim_op = struct
      let m = "Caramel.Core.Prim_op"

      let offset_int = (m, "offset_int")

      let is_outside_interval = (m, "is_outside_interval")
    end
  end
end

let rec of_lambda_prim ~unit:_ prim fields =
  match (prim, fields) with
  | Pmakeblock (_, _, _, Some block), _ -> of_lambda_block ~block ~fields
  | Pmakeblock (_, _, _, None), _ -> tuple_of_prim ~parts:fields
  | Pfield (idx, metadata), _ -> of_lambda_field ~idx ~metadata ~fields
  | Pccall prim, _ -> of_lambda_call ~prim ~fields
  (********************** other primitives *****************************)
  | Pintcomp op, _ ->
      let name =
        match op with
        | Cne -> Erlang.not_equal
        | Ceq -> Erlang.equal
        | Clt -> Erlang.less_than
        | Cgt -> Erlang.greater_than
        | Cle -> Erlang.less_or_equal
        | Cge -> Erlang.greater_or_equal
      in
      Ir.ext_call ~name ~args:fields
  | Pisint, _ -> Ir.ext_call ~name:Erlang.is_integer ~args:fields
  | Pisout, _ ->
      Ir.ext_call ~name:Caramel.Core.Prim_op.is_outside_interval ~args:fields
  | Poffsetint n, _ ->
      let fields = (Literal.int n |> Ir.lit) :: fields in
      Ir.ext_call ~name:Caramel.Core.Prim_op.offset_int ~args:fields
  | Praise _, _ -> Ir.ext_call ~name:Erlang.throw ~args:fields
  (* NOTE: Pgetglobal is used for a few things, including exception
     constructors in match failures, and external module accesses.
  *)
  | Pgetglobal id, _ when Identifier.is_match_failure id ->
      Ir.tuple
        ~parts:
          [ Literal.atom "EXIT" |> Ir.lit; Literal.atom "badmatch" |> Ir.lit ]
  | Pgetglobal id, [] when Identifier.of_ident id |> Identifier.is_module ->
      let id = Identifier.of_ident id in
      Ir.var id
  (********************** unsupported primitives *****************************)
  | Paddbint _, _ -> Error.todo "Paddbint"
  | Paddfloat, _ -> Error.todo "Paddfloat"
  | Psubfloat, _ -> Error.todo "Psubfloat"
  | Pmulfloat, _ -> Error.todo "Pmulfloat"
  | Pdivfloat, _ -> Error.todo "Pdivfloat"
  | Pandbint _, _ -> Error.todo "Pandbint"
  | Pandint, _ -> Error.todo "Pandint"
  | Porint, _ -> Error.todo "Porint"
  | Pxorint, _ -> Error.todo "Pxorint"
  | Parraylength _, _ -> Error.todo "Parraylength"
  | Parrayrefs _, _ -> Error.todo "Parrayrefs"
  | Parrayrefu _, _ -> Error.todo "Parrayrefu"
  | Parraysets _, _ -> Error.todo "Parraysets"
  | Parraysetu _, _ -> Error.todo "Parraysetu"
  | Pasrbint _, _ -> Error.todo "Pasrbint"
  | Pbbswap _, _ -> Error.todo "Pbbswap"
  | Pbigarraydim _, _ -> Error.todo "Pbigarraydim"
  | Pbigarrayref (_, _, _, _), _ -> Error.todo "Pbigarrayref"
  | Pbigarrayset (_, _, _, _), _ -> Error.todo "Pbigarrayset"
  | Pbigstring_load_16 _, _ -> Error.todo "Pbigstring_load_16"
  | Pbigstring_load_32 _, _ -> Error.todo "Pbigstring_load_32"
  | Pbigstring_load_64 _, _ -> Error.todo "Pbigstring_load_64"
  | Pbigstring_set_16 _, _ -> Error.todo "Pbigstring_set_16"
  | Pbigstring_set_32 _, _ -> Error.todo "Pbigstring_set_32"
  | Pbigstring_set_64 _, _ -> Error.todo "Pbigstring_set_64"
  | Pbintcomp (_, _), _ -> Error.todo "Pbintcomp"
  | Pbintofint _, _ -> Error.todo "Pbintofint"
  | Pbswap16, _ -> Error.todo "Pbswap16"
  | Pbytes_load_16 _, _ -> Error.todo "Pbytes_load_16"
  | Pbytes_load_32 _, _ -> Error.todo "Pbytes_load_32"
  | Pbytes_load_64 _, _ -> Error.todo "Pbytes_load_64"
  | Pbytes_of_string, _ -> Error.todo "Pbytes_of_string"
  | Pbytes_set_16 _, _ -> Error.todo "Pbytes_set_16"
  | Pbytes_set_32 _, _ -> Error.todo "Pbytes_set_32"
  | Pbytes_set_64 _, _ -> Error.todo "Pbytes_set_64"
  | Pbytes_to_string, _ -> Error.todo "Pbytes_to_string"
  | Pbyteslength, _ -> Error.todo "Pbyteslength"
  | Pbytesrefu, _ -> Error.todo "Pbytesrefu"
  | Pbytessetu, _ -> Error.todo "Pbytessetu"
  | Pbytesrefs, _ -> Error.todo "Pbytesrefs"
  | Pbytessets, _ -> Error.todo "Pbytessets"
  | Pcompare_ints, _ -> Error.todo "Pcompare_ints"
  | Pcompare_floats, _ -> Error.todo "Pcompare_floats"
  | Pcompare_bints _, _ -> Error.todo "Pcompare_bints"
  | Pctconst _, _ -> Error.todo "Pctconst"
  | Pcvtbint (_, _), _ -> Error.todo "Pcvtbint"
  | Pdivbint _, _ -> Error.todo "Pdivbint"
  | Pdivint _, _ -> Error.todo "Pdivint"
  | Pmodint _, _ -> Error.todo "Pmodint"
  | Pduparray (_, _), _ -> Error.todo "Pduparray"
  | Pduprecord (_, _), _ -> Error.todo "Pduprecord"
  | Pfield_computed, _ -> Error.todo "Pfield_computed"
  | Pfloatcomp _, _ -> Error.todo "Pfloatcomp"
  | Pfloatfield _, _ -> Error.todo "Pfloatfield"
  | Pgetglobal _, _ -> Error.todo "Pgetglobal"
  | Pignore, _ -> Error.todo "Pignore"
  | Pint_as_pointer, _ -> Error.todo "Pint_as_pointer"
  | Pintofbint _, _ -> Error.todo "Pintofbint"
  | Pintoffloat, _ -> Error.todo "Pintoffloat"
  | Pfloatofint, _ -> Error.todo "Pfloatofint"
  | Plslbint _, _ -> Error.todo "Plslbint"
  | Plslint, _ -> Error.todo "Plslint"
  | Plsrint, _ -> Error.todo "Plsrint"
  | Pasrint, _ -> Error.todo "Pasrint"
  | Plsrbint _, _ -> Error.todo "Plsrbint"
  | Pmakearray (_, _), _ -> Error.todo "Pmakearray"
  | Pmodbint _, _ -> Error.todo "Pmodbint"
  | Pmulbint _, _ -> Error.todo "Pmulbint"
  | Pnegbint _, _ -> Error.todo "Pnegbint"
  | Pnegfloat, _ -> Error.todo "Pnegfloat"
  | Pabsfloat, _ -> Error.todo "Pabsfloat"
  | Pnegint, _ -> Error.todo "Pnegint"
  | Paddint, _ -> Error.todo "Paddint"
  | Psubint, _ -> Error.todo "Psubint"
  | Pmulint, _ -> Error.todo "Pmulint"
  | Poffsetref _, _ -> Error.todo "Poffsetref"
  | Popaque, _ -> Error.todo "Popaque"
  | Porbint _, _ -> Error.todo "Porbint"
  | Psequand, _ -> Error.todo "Psequand"
  | Psequor, _ -> Error.todo "Psequor"
  | Pnot, _ -> Error.todo "Pnot"
  | Psetfield (_, _, _), _ -> Error.todo "Psetfield"
  | Psetfield_computed (_, _), _ -> Error.todo "Psetfield_computed"
  | Psetfloatfield (_, _), _ -> Error.todo "Psetfloatfield"
  | Psetglobal _, _ -> Error.todo "Psetglobal"
  | Pstring_load_16 _, _ -> Error.todo "Pstring_load_16"
  | Pstring_load_32 _, _ -> Error.todo "Pstring_load_32"
  | Pstring_load_64 _, _ -> Error.todo "Pstring_load_64"
  | Pstringlength, _ -> Error.todo "Pstringlength"
  | Pstringrefu, _ -> Error.todo "Pstringrefu"
  | Pstringrefs, _ -> Error.todo "Pstringrefs"
  | Psubbint _, _ -> Error.todo "Psubbint"
  | Pxorbint _, _ -> Error.todo "Pxorbint"

and of_lambda_call ~prim ~fields =
  match (prim.prim_name, fields) with
  | "%identity", [ field ] -> field
  | _ ->
      let name =
        (* TODO: move the module part of the call to an annotation *)
        match String.split_on_char ':' prim.prim_name with
        | [ m; f ] -> (m, f)
        | _ -> Error.todo "externals should have format M:F"
      in
      Ir.ext_call ~name ~args:fields

and of_lambda_block ~block ~fields =
  match block with
  | Block_record { fields = rfs; _ } -> record_of_prim ~fields ~record:rfs
  | Block_tuple -> tuple_of_prim ~parts:fields
  | Block_variant { label } -> variant_of_prim ~fields ~label
  | Block_construct { name; _ } -> construct_of_prim ~fields ~name

and of_lambda_field ~idx ~metadata ~fields =
  match (metadata, fields) with
  (* NOTE: we expect field access to _have_ a single field *)
  | Some (Field_record { name; _ }), [ expr ] ->
      let field = Some (Ir.lit (Literal.string name)) in
      Ir.field ~idx ~field ~expr
  | Some (Field_constructor { name = "::"; _ }), [ expr ] -> (
      match idx with
      | 0 -> Ir.ext_call ~name:Erlang.hd ~args:[ expr ]
      | 1 -> Ir.ext_call ~name:Erlang.tl ~args:[ expr ]
      | _ -> Error.panic "tried to access a list outside of head/tail")
  | Some (Field_constructor _), [ expr ] ->
      (* NOTE:

         On trying to access the value inside an `Ok(x)` (regular variant),
         we are really trying to call `erlang:element(?, {ok, X})`.

         Since the OCaml expression we have at this point looks like:

           [ 0: "Ok"; 1: "value" ]

         You'd think we want `erlang:element(1, {ok, X})`, but you'd be surprised.

         The `erlang:element/2` function is 1-indexed, so what we really want is
         `erlang:elemnet(2, {ok, X})` to get that `X` out.

         Since our `idx` is zero-indexed, this leaves us 2 slots away.

         Hence, `idx + 2` will be the right position.
      *)
      let pos = Ir.lit (Literal.int (idx + 2)) in
      Ir.ext_call ~name:Erlang.element ~args:[ pos; expr ]
  | Some (Field_module _), [ expr ] ->
      (* NOTE: this is currently implemented as a pass in pass_ext_calls.ml *)
      Ir.field ~idx ~field:None ~expr
  | Some (Field_primitive { mod_name; prim_name; _ }), [ expr ] ->
      (* NOTE: are we actually using this? *)
      Ir.ext_call ~name:(mod_name, prim_name) ~args:[ expr ]
  | None, [ expr ] ->
      (* NOTE: Read the note above first.

         The main difference is that for anonymous tuples, we don't need an
         additional index shift since we don't have to skip the constructor tag.
      *)
      let pos = Ir.lit (Literal.int (idx + 1)) in
      Ir.ext_call ~name:Erlang.element ~args:[ pos; expr ]
  | _, _ ->
      Logs.debug (fun f ->
          f "field access should happen on a single expression");
      assert false

(*
  Handle translating an OCaml record, either inlined or not, into our IR.

  We choose a proplist representation here and save all the keys and values
  as a list.
*)
and record_of_prim ~fields ~record =
  let fields =
    List.mapi
      (fun idx v ->
        let field = record.(idx) in
        let k = Ir.lit (Literal.atom field.brf_name) in
        (k, v))
      fields
  in
  Ir.record ~fields

and tuple_of_prim ~parts = Ir.tuple ~parts

and variant_of_prim ~fields ~label =
  let tag = Ir.lit (Literal.atom (String.lowercase_ascii label)) in
  Ir.tuple ~parts:(tag :: fields)

and construct_of_prim ~fields ~name =
  match (name, fields) with
  | "::", [ h; t ] -> Ir.cons h t
  | _, _ ->
      let tag = Ir.lit (Literal.atom (String.lowercase_ascii name)) in
      Ir.tuple ~parts:(tag :: fields)
