module Caml = Ast_helper
open Parsetree

module Error = struct
  type t = [ `Unsupported_constant of Parsetree.literal ] [@@deriving sexp]

  exception Translate_error of t

  let pp ppf error =
    let sexp = sexp_of_t error in
    Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

  let err e = raise (Translate_error e)
end

let str_to_caml_lid name =
  Location.{ txt = Longident.Lident name; loc = Location.none }

let to_caml_lid (Id id) = str_to_caml_lid id

let to_caml_ident (Id id) = Ident.create_local id

let to_caml_loc (Id id) = Location.mkloc id !Caml.default_loc

let rec to_caml_core_type t =
  match t with
  | Type_name id -> Caml.Typ.constr (to_caml_lid id) []
  | Type_var name -> Caml.Typ.var name
  | Type_apply (id, args) ->
      Caml.Typ.constr (to_caml_lid id) (List.map to_caml_core_type args)
  | Type_arrow (a, b) ->
      Caml.Typ.arrow Asttypes.Nolabel (to_caml_core_type a)
        (to_caml_core_type b)

let to_caml_type { typ_name; typ_desc; _ } =
  let desc =
    match typ_desc with
    | Type_abstract -> Caml.Type.mk ~kind:Ptype_abstract (to_caml_loc typ_name)
    | Type_variant { constructors } ->
        let constructors =
          List.map
            (fun (ctr : constructor_decl) ->
              let args =
                match ctr.ctr_args with
                | Tuple def ->
                    Caml.Type.tuple_constructor_argument
                      (List.map to_caml_core_type def)
                | Record labels ->
                    let labels =
                      List.map
                        (fun (lbl : label_decl) ->
                          Caml.Type.field (to_caml_loc lbl.lbl_name)
                            (to_caml_core_type lbl.lbl_type))
                        labels
                    in
                    Caml.Type.record_constructor_argument labels
              in
              Caml.Type.constructor ~args (to_caml_loc ctr.ctr_name))
            constructors
        in
        Caml.Type.mk ~kind:(Ptype_variant constructors) (to_caml_loc typ_name)
  in
  Caml.Str.type_ Asttypes.Recursive [ desc ]

let to_caml_arg_label label =
  match label with
  | No_label -> Asttypes.Nolabel
  | Label str -> Asttypes.Labelled str
  | Optional str -> Asttypes.Optional str

let to_caml_const lit =
  match lit with
  | Lit_string str -> Caml.Const.string str
  | Lit_atom _atom -> Error.err (`Unsupported_constant lit)

let rec to_caml_pat pat =
  match pat with
  | Pat_any -> Caml.Pat.any ()
  | Pat_nil -> Caml.Pat.construct (str_to_caml_lid "[]") None
  | Pat_cons (h, t) ->
      Caml.Pat.construct (str_to_caml_lid "::")
        (Some ([], Caml.Pat.tuple [ to_caml_pat h; to_caml_pat t ]))
  | Pat_tuple [] -> Caml.Pat.construct (str_to_caml_lid "()") None
  | Pat_tuple parts -> Caml.Pat.tuple (List.map to_caml_pat parts)
  | Pat_bind id -> Caml.Pat.var (to_caml_loc id)

let rec to_caml_expr exp =
  match exp with
  | Expr_nil -> Caml.Exp.construct (str_to_caml_lid "[]") None
  | Expr_cons (h, t) ->
      Caml.Exp.construct (str_to_caml_lid "::")
        (Some (Caml.Exp.tuple [ to_caml_expr h; to_caml_expr t ]))
  | Expr_var id -> Caml.Exp.ident (to_caml_lid id)
  | Expr_tuple parts -> Caml.Exp.tuple (List.map to_caml_expr parts)
  | Expr_literal (Lit_atom atom) -> Caml.Exp.variant atom None
  | Expr_literal lit -> Caml.Exp.constant (to_caml_const lit)
  | Expr_match (expr, cases) ->
      Caml.Exp.match_ (to_caml_expr expr) (List.map to_caml_case cases)
  | Expr_call (fn, args) ->
      Caml.Exp.apply (to_caml_expr fn)
        (List.map (fun arg -> (Asttypes.Nolabel, to_caml_expr arg)) args)
  | Expr_seq (e1, e2) -> Caml.Exp.sequence (to_caml_expr e1) (to_caml_expr e2)

and to_caml_case { cs_lhs; cs_rhs } =
  Caml.Exp.case (to_caml_pat cs_lhs) (to_caml_expr cs_rhs)

let to_caml_external { ext_name; ext_type; ext_symbol; _ } =
  Caml.Str.primitive
    (Caml.Val.mk ~prim:[ ext_symbol ] (to_caml_loc ext_name)
       (to_caml_core_type ext_type))

let to_caml_value_binding { fn_name; fn_args; fn_body; _ } =
  let name = Caml.Pat.var (to_caml_loc fn_name) in
  let rec build_fun args =
    match args with
    | [] -> to_caml_expr fn_body
    | (label, pat) :: args ->
        let default_value = None in
        Caml.Exp.fun_ (to_caml_arg_label label) default_value (to_caml_pat pat)
          (build_fun args)
  in
  let fun_ =
    match fn_args with
    | [] -> build_fun [ (No_label, Pat_tuple []) ]
    | _ -> build_fun fn_args
  in
  Caml.Str.value Asttypes.Recursive [ Caml.Vb.mk name fun_ ]

let to_caml_str_item item =
  match item with
  | Str_type t -> to_caml_type t
  | Str_fun f -> to_caml_value_binding f
  | Str_extern e -> to_caml_external e

let to_caml_parsetree parsetree = List.map to_caml_str_item parsetree
