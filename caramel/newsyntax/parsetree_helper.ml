open Parsetree

let id str = Id str

module Visibility = struct
  let pub = Public

  let priv = Private
end

module Expr = struct
  let nil = Expr_nil

  let var id = Expr_var id

  let tuple ~parts = Expr_tuple parts

  let case ~lhs ~rhs = { cs_lhs = lhs; cs_rhs = rhs }

  let match_ ~expr ~cases = Expr_match (expr, cases)

  let list ~head ~tail = Expr_cons (head, tail)

  let lit_str s = Expr_literal (Lit_string s)

  let lit_atom a = Expr_literal (Lit_atom a)

  let call ~name ~args = Expr_call (name, args)

  let seq a b = Expr_seq (a, b)
end

module Typ = struct
  let id name = Type_name name

  let var name = Type_var name

  let arrow a b = Type_arrow (a, b)

  let apply ~id ~args = Type_apply (id, args)
end

module Type = struct
  let variant ~constructors = Type_variant { constructors }

  let variant_constructor ~name ~args ~annot =
    { ctr_name = name; ctr_args = args; ctr_annot = annot }

  let variant_record_args ~labels = Record labels

  let variant_tuple_args ~parts = Tuple parts

  let label_decl ~name ~type_ ~annot =
    { lbl_name = name; lbl_type = type_; lbl_annot = annot }

  let mk ~name ~desc ~annot =
    { typ_name = name; typ_desc = desc; typ_annot = annot }
end

module Pat = struct
  let any = Pat_any

  let nil = Pat_nil

  let bind name = Pat_bind name

  let tuple ~parts = Pat_tuple parts

  let list ~head ~tail = Pat_cons (head, tail)
end

module Annot = struct
  let field ~name ~value = (name, value)

  let map ~fields = Map fields

  let mk ~name ~desc = { ann_name = name; ann_desc = desc }
end

module Fun_decl = struct
  let mk ~name ~visibility ~annot ~args ~body =
    {
      fn_name = name;
      fn_visibility = visibility;
      fn_annot = annot;
      fn_args = args;
      fn_arity = List.length args;
      fn_body = body;
    }
end

module Ext = struct
  let mk ~name ~type_sig ~symbol ~annot ~visibility =
    {
      ext_name = name;
      ext_type = type_sig;
      ext_symbol = symbol;
      ext_annot = annot;
      ext_visibility = visibility;
    }
end

module Str = struct
  let type_ t = Str_type t

  let fun_ fn = Str_fun fn

  let extern e = Str_extern e
end
