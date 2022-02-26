open Sexplib.Std

type id = Id of string list [@@deriving sexp]

type visibility = Public | Private [@@deriving sexp]

type literal =
  | Lit_integer of string
  | Lit_string of string
  | Lit_atom of string
[@@deriving sexp]

type annotation = { ann_name : id; ann_desc : annotation_desc option }
[@@deriving sexp]

and annotation_desc = Map of (id * expr option) list [@@deriving sexp]

and quasiquote =
  | Quasiquote of Token.t list
  | Unquote of expr
  | Unquote_splicing of expr
[@@deriving sexp]

and expr =
  | Expr_field of expr * id
  | Expr_record of (id * expr) list
  | Expr_constructor of id * constructor_expr
  | Expr_lambda of (arg_label * pattern) list * expr
  | Expr_open of id * expr
  | Expr_call of expr * expr list
  | Expr_let of pattern * expr * expr
  | Expr_cons of expr * expr
  | Expr_literal of literal
  | Expr_match of expr * case list
  | Expr_nil
  | Expr_seq of expr * expr
  | Expr_tuple of expr list
  | Expr_var of id
  | Expr_quote of quasiquote list
[@@deriving sexp]

and pattern =
  | Pat_any
  | Pat_bind of id
  | Pat_cons of pattern * pattern
  | Pat_nil
  | Pat_tuple of pattern list
  | Pat_literal of literal
  | Pat_record of (id * pattern) list * exhaustive
  | Pat_constructor of id * constructor_pat
[@@deriving sexp]

and exhaustive = Exhaustive | Partial

and constructor_pat =
  | Ctp_record of (id * pattern) list * exhaustive
  | Ctp_tuple of pattern list
[@@deriving sexp]

and constructor_expr = Ctr_record of (id * expr) list | Ctr_tuple of expr list
[@@deriving sexp]

and case = { cs_lhs : pattern; cs_rhs : expr } [@@deriving sexp]

and type_expr =
  | Type_tuple of type_expr list
  | Type_arrow of type_expr * type_expr
  | Type_apply of id * type_expr list
  | Type_name of id
  | Type_var of string
[@@deriving sexp]

and type_decl = {
  typ_name : id;
  typ_args : string list;
  typ_desc : type_kind;
  typ_annot : annotation list;
}
[@@deriving sexp]

and type_kind =
  | Type_abstract
  | Type_variant of { tyk_constructors : constructor_decl list }
  | Type_record of { tyk_labels : label_decl list }
  | Type_alias of type_expr
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

and mod_decl = {
  mod_name : id;
  mod_items : structure_item list;
  mod_visibility : visibility;
  mod_annot : annotation list;
}
[@@deriving sexp]

and mod_expr = Mod_open of id | Mod_decl of mod_decl [@@deriving sexp]

and structure_item =
  | Str_type of type_decl
  | Str_fun of fun_decl
  | Str_macro of fun_decl
  | Str_extern of extern_decl
  | Str_mod_expr of mod_expr
  | Str_comment of string
[@@deriving sexp]

and t = structure_item list [@@deriving sexp]

(*** Pretty printers ***********************************************************)

let pp ppf t =
  let sexp = sexp_of_t t in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let pp_id ppf id =
  let sexp = sexp_of_id id in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let pp_expr ppf expr =
  let sexp = sexp_of_expr expr in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let pp_exprs ppf expr =
  let sexp = sexp_of_expr (Expr_tuple expr) in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let pp_annot ppf annot =
  let sexp = sexp_of_annotation annot in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let pp_pat ppf pat =
  let sexp = sexp_of_pattern pat in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let pp_quote ppf quote =
  let sexp = sexp_of_expr (Expr_quote quote) in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp
