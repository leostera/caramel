open Sexplib.Std

(* Parametrizable Abstract Syntax Tree *)
type 'ctx atom = Atom of 'ctx * string [@@deriving sexp]

type 'ctx comment = Comment of 'ctx * string [@@deriving sexp]

type 'ctx qualified_name = { qn_mod : 'ctx name; qn_name : 'ctx name }

and 'ctx name =
  | Name_var of 'ctx * string
  | Name_atom of 'ctx atom
  | Name_qualified of 'ctx * 'ctx qualified_name
[@@deriving sexp]

and 'ctx term =
  | Term_atom of 'ctx atom
  | Term_binary_string of 'ctx * 'ctx term_binary
  | Term_char of 'ctx * string
  | Term_float of 'ctx * string
  | Term_integer of 'ctx * string
  | Term_list of 'ctx term_list
  | Term_tuple of 'ctx term_tuple
  | Term_map of 'ctx term_map
  | Term_record of 'ctx term_record
  | Term_string of string
[@@deriving sexp]

and 'ctx term_binary = 'ctx term_binary_part list [@@deriving sexp]

and 'ctx term_binary_part = {
  binpart_value : 'ctx expression;
  binpart_sized : 'ctx term option;
  binpart_types : 'ctx term_binary_part_type list;
}

and 'ctx term_binary_part_type =
  | Binpart_type_big
  | Binpart_type_binary
  | Binpart_type_bits
  | Binpart_type_bitstring
  | Binpart_type_bytes
  | Binpart_type_float
  | Binpart_type_integer
  | Binpart_type_little
  | Binpart_type_native
  | Binpart_type_signed
  | Binpart_type_unsigned
  | Binpart_type_utf16
  | Binpart_type_utf32
  | Binpart_type_utf8
[@@deriving sexp]

and 'ctx term_list = 'ctx expression list
[@@deriving sexp]

and 'ctx term_tuple = { size : int; elements : 'ctx expression list }
[@@deriving sexp]

and 'ctx term_map = 'ctx term_map_field list [@@deriving sexp]

and 'ctx term_map_field = {
  mf_name : 'ctx expression;
  mf_value : 'ctx expression;
}
[@@deriving sexp]

and 'ctx term_record = {
  rec_name : 'ctx name;
  rec_elements : 'ctx term_map_field list;
}
[@@deriving sexp]

and 'ctx module_attribute = { atr_name : 'ctx atom; atr_value : 'ctx expression }
[@@deriving sexp]

and 'ctx fun_decl = {
  fd_name : 'ctx atom;
  fd_arity : int;
  fd_cases : 'ctx case list;
}
[@@deriving sexp]

and 'ctx case = {
  c_lhs : 'ctx pattern list;
  (* c_guard : 'ctx guard option; *)
  c_rhs : 'ctx expression;
}
[@@deriving sexp]

and 'ctx expression =
  | Expr_case of 'ctx expr_case
  | Expr_var of 'ctx name
  | Expr_term of 'ctx term
  | Expr_macro_app of 'ctx expr_macro_app
  | Expr_fun_call of 'ctx expr_fun_call
  | Expr_lambda of 'ctx expr_lambda
  | Expr_match of 'ctx expr_match
[@@deriving sexp]

and 'ctx expr_case = 'ctx expression * 'ctx case list [@@deriving sexp]

and 'ctx expr_macro_app = {
  mapp_name : string;
  mapp_args : 'ctx expression list;
}
[@@deriving sexp]

and 'ctx expr_fun_call = {
  fncall_name : 'ctx expression;
  fncall_args : 'ctx expression list;
}
[@@deriving sexp]

and 'ctx expr_lambda = 'ctx case list [@@deriving sexp]

and 'ctx expr_match = {
  match_lhs : 'ctx expression;
  match_rhs : 'ctx expression;
}
[@@deriving sexp]

and 'ctx pattern =
  | Pat_term of 'ctx term
  | Pat_var of 'ctx name
  | Pat_list of 'ctx pat_list
  | Pat_tuple of 'ctx pat_tuple

and 'ctx pat_list = 'ctx pattern list
[@@deriving sexp]

and 'ctx pat_tuple = { ptup_size : int; ptup_elements : 'ctx pattern list }
[@@deriving sexp]

and 'ctx structure_item =
  | Comment of 'ctx comment
  | Mod_attr of 'ctx module_attribute
  | Fun_decl of 'ctx fun_decl
  | Expr of 'ctx expression
[@@deriving sexp]

and 'ctx module_structure = 'ctx structure_item list [@@deriving sexp]

and 'ctx implementation = {
  behaviours : 'ctx atom list;
  module_name : 'ctx atom;
  attributes : 'ctx module_attribute list;
  functions : 'ctx fun_decl list;
}
[@@deriving sexp]


(*** Parsetree ***)

type parse_node = { txt : string; pos : int * int }

type parsetree = parse_node module_structure
