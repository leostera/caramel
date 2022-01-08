open Sexplib.Std

type id = Id of string [@@deriving sexp]

type visibility = Public | Private [@@deriving sexp]

type literal = Lit_string of string | Lit_atom of string [@@deriving sexp]

type annotation = { ann_name : id; ann_desc : annotation_desc option }
[@@deriving sexp]

and annotation_desc = Map of (id * string option) list [@@deriving sexp]

and expr =
  | Expr_call of expr * expr list
  | Expr_cons of expr * expr
  | Expr_literal of literal
  | Expr_match of expr * case list
  | Expr_nil
  | Expr_seq of expr * expr
  | Expr_tuple of expr list
  | Expr_var of id
[@@deriving sexp]

and pattern =
  | Pat_any
  | Pat_bind of id
  | Pat_cons of pattern * pattern
  | Pat_nil
  | Pat_tuple of pattern list
[@@deriving sexp]

and case = { cs_lhs : pattern; cs_rhs : expr } [@@deriving sexp]

and type_expr =
  | Type_arrow of type_expr * type_expr
  | Type_apply of id * type_expr list
  | Type_name of id
  | Type_var of string
[@@deriving sexp]

and type_decl = {
  typ_name : id;
  typ_desc : type_kind;
  typ_annot : annotation list;
}
[@@deriving sexp]

and type_kind =
  | Type_abstract
  | Type_variant of { constructors : constructor_decl list }
[@@deriving sexp]

and constructor_decl = {
  ctr_name : id;
  ctr_args : constructor_args;
  ctr_annot : annotation list;
}
[@@deriving sexp]

and constructor_args = Record of label_decl list | Tuple of type_expr list
[@@deriving sexp]

and label_decl = {
  lbl_name : id;
  lbl_type : type_expr;
  lbl_annot : annotation list;
}
[@@deriving sexp]

and extern_decl = {
  ext_name : id;
  ext_type : type_expr;
  ext_symbol : string;
  ext_visibility : visibility;
  ext_annot : annotation list;
}
[@@deriving sexp]

and fun_decl = {
  fn_visibility : visibility;
  fn_name : id;
  fn_args : (arg_label * pattern) list;
  fn_arity : int;
  fn_body : expr;
  fn_annot : annotation list;
}
[@@deriving sexp]

and arg_label = No_label | Label of string | Optional of string
[@@deriving sexp]

and structure_item =
  | Str_type of type_decl
  | Str_fun of fun_decl
  | Str_extern of extern_decl
[@@deriving sexp]

and t = structure_item list [@@deriving sexp]

(*** Ops on t ******************************************************************)

let pp ppf t =
  let sexp = sexp_of_t t in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp
