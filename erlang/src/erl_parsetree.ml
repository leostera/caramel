open Sexplib.Std

(* Parametrizable Abstract Syntax Tree *)
type 'ctx atom = Atom of 'ctx * string [@@deriving sexp]

type 'ctx comment = Comment of 'ctx * string [@@deriving sexp]

type 'ctx qualified_name = { qn_mod : 'ctx name; qn_name : 'ctx name }

and 'ctx name = Name_var of 'ctx * string | Name_atom of 'ctx atom
[@@deriving sexp]

and 'ctx term =
  | Term_atom of 'ctx atom
  | Term_binary_string of 'ctx * 'ctx term_binary
  | Term_char of 'ctx * char
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

and 'ctx term_list = 'ctx expression list [@@deriving sexp]

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

and 'ctx module_attribute =
  | Export_type of { attr_type_name : 'ctx atom; attr_type_arity : int }
  | Export_fun of { attr_fun_name : 'ctx atom; attr_fun_arity : int }
  | Custom of { attr_name : 'ctx atom; attr_value : 'ctx expression }
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
  | Pat_atom of 'ctx atom
  | Pat_binary_string of 'ctx * 'ctx term_binary
  | Pat_char of 'ctx * char
  | Pat_float of 'ctx * string
  | Pat_ignore
  | Pat_integer of 'ctx * string
  | Pat_list of 'ctx pat_list
  | Pat_map of 'ctx pat_map
  | Pat_record of 'ctx pat_record
  | Pat_string of string
  | Pat_tuple of 'ctx pat_tuple
  | Pat_var of 'ctx name

and 'ctx pat_list = 'ctx pattern list [@@deriving sexp]

and 'ctx pat_tuple = { ptup_size : int; ptup_elements : 'ctx pattern list }
[@@deriving sexp]

and 'ctx pat_map = 'ctx pat_map_field list [@@deriving sexp]

and 'ctx pat_map_field = {
  pmf_kind : [ `Mandatory | `Optional ];
  pmf_name : 'ctx expression;
  pmf_value : 'ctx expression;
}
[@@deriving sexp]

and 'ctx pat_record = {
  prec_name : 'ctx name;
  prec_elements : 'ctx pat_map_field list;
}
[@@deriving sexp]

and 'ctx structure_item =
  | Comment of 'ctx comment
  | Mod_attr of 'ctx module_attribute
  | Fun_decl of 'ctx fun_decl
[@@deriving sexp]

and 'ctx module_structure = 'ctx structure_item list [@@deriving sexp]

and ('ctx, 'mod_ctx) implementation = {
  attributes : 'ctx module_attribute list;
  behaviours : 'ctx atom list;
  functions : 'ctx fun_decl list;
  module_name : 'ctx atom;
  file_name : string;
  ctx : 'ctx;
  mod_ctx : 'mod_ctx;
}
[@@deriving sexp]

(*** Parsetree ***)

type location = { txt : string; pos : int * int }

type parsetree = location module_structure
