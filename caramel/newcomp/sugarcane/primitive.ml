open Lambda

module Erlang = struct
  let m = "erlang"

  let not_equal = (m, "=/=")

  let equal = (m, "=:=")

  let is_integer = (m, "is_integer")
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

(* NOTE(@ostera): Fuck it, since we lost most of the type information by the
   time we try to decode these primitives, particularly for the Pmakeblock
   variant, we will encode everything as Erlang maps.
*)
let of_lambda_prim ~unit:_ prim fields =
  match (prim, fields) with
  | Pmakeblock (_, _, _), _ ->
      let fields = List.mapi (fun idx f -> (idx, f)) fields in
      Ir.record ~fields
  (* NOTE: we expect field access to _have_ a single field *)
  | Pfield idx, [ expr ] -> Ir.field ~idx ~expr
  | Pccall prim, _ ->
      let name =
        match String.split_on_char ':' prim.prim_name with
        | [ m; f ] -> (m, f)
        | _ -> Error.todo "externals should have format M:F"
      in
      Ir.ext_call ~name ~args:fields
  | Pintcomp Cne, _ -> Ir.ext_call ~name:Erlang.not_equal ~args:fields
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
  | Pfield _, _ -> Error.todo "Pfield"
  | Pfield_computed, _ -> Error.todo "Pfield_computed"
  | Pfloatcomp _, _ -> Error.todo "Pfloatcomp"
  | Pfloatfield _, _ -> Error.todo "Pfloatfield"
  | Pgetglobal _, _ -> Error.todo "Pgetglobal"
  | Pignore, _ -> Error.todo "Pignore"
  | Pint_as_pointer, _ -> Error.todo "Pint_as_pointer"
  | Pintcomp _, _ -> Error.todo "Pintcomp"
  | Pintofbint _, _ -> Error.todo "Pintofbint"
  | Pintoffloat, _ -> Error.todo "Pintoffloat"
  | Pfloatofint, _ -> Error.todo "Pfloatofint"
  | Pisint, _ -> Ir.ext_call ~name:Erlang.is_integer ~args:fields
  | Pisout, _ ->
      Ir.ext_call ~name:Caramel.Core.Prim_op.is_outside_interval ~args:fields
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
  | Poffsetint n, _ ->
      let fields = (Literal.int n |> Ir.lit) :: fields in
      Ir.ext_call ~name:Caramel.Core.Prim_op.offset_int ~args:fields
  | Poffsetref _, _ -> Error.todo "Poffsetref"
  | Popaque, _ -> Error.todo "Popaque"
  | Porbint _, _ -> Error.todo "Porbint"
  | Praise _, _ -> Error.todo "Praise"
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
