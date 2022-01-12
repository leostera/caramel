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
  Location.
    {
      txt =
        (* NOTE: should we handle this earlier on ourselves? *)
        (match Longident.unflatten name with
        | Some name -> name
        | None -> Longident.Lident (String.concat "." name));
      loc = Location.none;
    }

let to_caml_lid (Id id) = str_to_caml_lid id

let to_caml_loc (Id id) =
  Location.mkloc (String.concat "." id) !Caml.default_loc

let cons = str_to_caml_lid [ "::" ]

let nil = str_to_caml_lid [ "[]" ]

let unit = str_to_caml_lid [ "()" ]

let rec to_caml_core_type t =
  match t with
  | Type_name id -> Caml.Typ.constr (to_caml_lid id) []
  | Type_tuple parts -> Caml.Typ.tuple (List.map to_caml_core_type parts)
  | Type_var name -> Caml.Typ.var name
  | Type_apply (id, args) ->
      Caml.Typ.constr (to_caml_lid id) (List.map to_caml_core_type args)
  | Type_arrow (a, b) ->
      Caml.Typ.arrow Asttypes.Nolabel (to_caml_core_type a)
        (to_caml_core_type b)

let to_caml_type { typ_name; typ_desc; typ_args; _ } =
  let typ_name = to_caml_loc typ_name in
  let params =
    List.map
      (fun arg ->
        (Caml.Typ.var arg, (Asttypes.NoVariance, Asttypes.NoInjectivity)))
      typ_args
  in
  let desc =
    match typ_desc with
    | Type_alias id ->
        Caml.Type.mk ~kind:Ptype_abstract ~params
          ~manifest:(Caml.Typ.constr (to_caml_lid id) [])
          typ_name
    | Type_abstract -> Caml.Type.mk ~kind:Ptype_abstract ~params typ_name
    | Type_record { tyk_labels } ->
        let labels =
          List.map
            (fun (lbl : label_decl) ->
              Caml.Type.field (to_caml_loc lbl.lbl_name)
                (to_caml_core_type lbl.lbl_type))
            tyk_labels
        in
        Caml.Type.mk ~kind:(Ptype_record labels) ~params typ_name
    | Type_variant { tyk_constructors } ->
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
            tyk_constructors
        in
        Caml.Type.mk ~kind:(Ptype_variant constructors) ~params typ_name
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
  | Lit_integer i -> Caml.Const.integer i
  | Lit_atom _atom -> Error.err (`Unsupported_constant lit)

let rec to_caml_pat pat =
  match pat with
  | Pat_any -> Caml.Pat.any ()
  | Pat_nil -> Caml.Pat.construct nil None
  | Pat_cons (h, t) ->
      Caml.Pat.construct cons
        (Some ([], Caml.Pat.tuple [ to_caml_pat h; to_caml_pat t ]))
  | Pat_tuple [] -> Caml.Pat.construct unit None
  | Pat_tuple parts -> Caml.Pat.tuple (List.map to_caml_pat parts)
  | Pat_bind id -> Caml.Pat.var (to_caml_loc id)
  | Pat_literal (Lit_atom atom) ->
      Caml.Pat.construct (str_to_caml_lid [ atom ]) None
  | Pat_literal lit -> Caml.Pat.constant (to_caml_const lit)
  | Pat_constructor (name, Ctp_tuple []) ->
      Caml.Pat.construct (to_caml_lid name) None
  | Pat_constructor (name, Ctp_tuple [ part ]) ->
      Caml.Pat.construct (to_caml_lid name)
        (Some ([], Caml.Pat.tuple [ to_caml_pat part ]))
  | Pat_constructor (name, Ctp_tuple parts) ->
      Caml.Pat.construct (to_caml_lid name)
        (Some ([], Caml.Pat.tuple (List.map to_caml_pat parts)))
  | Pat_constructor (name, Ctp_record fields) ->
      Caml.Pat.construct (to_caml_lid name)
        (Some
           ( List.map (fun (k, _) -> to_caml_loc k) fields,
             Caml.Pat.tuple (List.map (fun (_, v) -> to_caml_pat v) fields) ))
  | Pat_record fields ->
      Caml.Pat.record
        (List.map (fun (k, v) -> (to_caml_lid k, to_caml_pat v)) fields)
        Asttypes.Closed

let rec to_caml_expr exp =
  match exp with
  | Expr_let (pat, body, expr) ->
      let binding = Caml.Vb.mk (to_caml_pat pat) (to_caml_expr body) in
      Caml.Exp.let_ Asttypes.Nonrecursive [ binding ] (to_caml_expr expr)
  | Expr_nil -> Caml.Exp.construct nil None
  | Expr_cons (h, t) ->
      Caml.Exp.construct cons
        (Some (Caml.Exp.tuple [ to_caml_expr h; to_caml_expr t ]))
  | Expr_var id -> Caml.Exp.ident (to_caml_lid id)
  | Expr_constructor (name, Ctr_tuple []) ->
      Caml.Exp.construct (to_caml_lid name) None
  | Expr_constructor (name, Ctr_tuple [ part ]) ->
      Caml.Exp.construct (to_caml_lid name) (Some (to_caml_expr part))
  | Expr_constructor (name, Ctr_tuple parts) ->
      Caml.Exp.construct (to_caml_lid name)
        (Some (Caml.Exp.tuple (List.map to_caml_expr parts)))
  | Expr_constructor (name, Ctr_record fields) ->
      Caml.Exp.construct (to_caml_lid name)
        (Some
           (Caml.Exp.record
              (List.map (fun (k, v) -> (to_caml_lid k, to_caml_expr v)) fields)
              None))
  | Expr_record fields ->
      Caml.Exp.record
        (List.map (fun (k, v) -> (to_caml_lid k, to_caml_expr v)) fields)
        None
  | Expr_tuple [] -> Caml.Exp.construct unit None
  | Expr_tuple parts -> Caml.Exp.tuple (List.map to_caml_expr parts)
  | Expr_literal (Lit_atom atom) -> Caml.Exp.variant atom None
  | Expr_literal lit -> Caml.Exp.constant (to_caml_const lit)
  | Expr_match (expr, cases) ->
      Caml.Exp.match_ (to_caml_expr expr) (List.map to_caml_case cases)
  | Expr_lambda (args, body) -> build_lambda args body
  | Expr_call (fn, []) ->
      Caml.Exp.apply (to_caml_expr fn)
        [ (Asttypes.Nolabel, Caml.Exp.construct unit None) ]
  | Expr_call (fn, args) ->
      Caml.Exp.apply (to_caml_expr fn)
        (List.map (fun arg -> (Asttypes.Nolabel, to_caml_expr arg)) args)
  | Expr_seq (e1, e2) -> Caml.Exp.sequence (to_caml_expr e1) (to_caml_expr e2)
  | Expr_open (mod_name, expr) ->
      let mod_id = Caml.Mod.ident (to_caml_lid mod_name) in
      let open_desc = Caml.Exp.open_declaration mod_id in
      Caml.Exp.open_ open_desc (to_caml_expr expr)
  | Expr_field (expr, id) -> Caml.Exp.field (to_caml_expr expr) (to_caml_lid id)
  | Expr_quote _ | Expr_unquote _ -> Caml.Exp.unreachable ()

and to_caml_case { cs_lhs; cs_rhs } =
  Caml.Exp.case (to_caml_pat cs_lhs) (to_caml_expr cs_rhs)

and build_lambda args body =
  let rec build_fun args =
    match args with
    | [] -> to_caml_expr body
    | (label, pat) :: args ->
        let default_value = None in
        Caml.Exp.fun_ (to_caml_arg_label label) default_value (to_caml_pat pat)
          (build_fun args)
  in
  match args with
  | [] -> build_fun [ (No_label, Pat_tuple []) ]
  | _ -> build_fun args

let to_caml_external { ext_name; ext_type; ext_symbol; _ } =
  Caml.Str.primitive
    (Caml.Val.mk ~prim:[ ext_symbol ] (to_caml_loc ext_name)
       (to_caml_core_type ext_type))

let to_caml_value_binding { fn_name; fn_args; fn_body; _ } =
  let name = Caml.Pat.var (to_caml_loc fn_name) in
  let fun_ = build_lambda fn_args fn_body in
  Caml.Str.value Asttypes.Recursive [ Caml.Vb.mk name fun_ ]

let rec to_caml_str_item item =
  match item with
  | Str_type t -> Some (to_caml_type t)
  | Str_fun f -> Some (to_caml_value_binding f)
  | Str_extern e -> Some (to_caml_external e)
  | Str_mod_expr (Mod_decl m) -> Some (to_caml_module m)
  | Str_mod_expr (Mod_open m) -> Some (to_caml_module_open m)
  | Str_comment _ -> None
  | Str_macro _ -> None

and to_caml_module m =
  let str = List.filter_map to_caml_str_item m.mod_items in
  let mod_desc = Caml.Mod.mod_desc_structure str in
  let mod_expr = Caml.Mod.mk mod_desc in
  let mod_name =
    let (Id name) = m.mod_name in
    Location.{ txt = Some (String.concat "." name); loc = Location.none }
  in
  let mod_binding = Caml.Mb.mk mod_name mod_expr in
  Caml.Str.module_ mod_binding

and to_caml_module_open mod_name =
  let mod_id = Caml.Mod.ident (to_caml_lid mod_name) in
  let open_desc = Caml.Exp.open_declaration mod_id in
  Caml.Str.open_ open_desc

let to_caml_parsetree parsetree = List.filter_map to_caml_str_item parsetree
