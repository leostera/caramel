open Sexplib.Std
open Asttypes

type t =
  | Lit_atom of string
  | Lit_int of string
  | Lit_char of string
  | Lit_string of string
  | Lit_float of string
[@@deriving sexp]

let string str = Lit_string str

let atom str = Lit_atom str

let int i = Lit_int (Int.to_string i)

let t = Lit_int "1"

let f = Lit_int "0"

let of_const c =
  match c with
  | Const_int32 i -> Lit_int (Int32.to_string i)
  | Const_int64 i -> Lit_int (Int64.to_string i)
  | Const_nativeint i -> Lit_int (Nativeint.to_string i)
  | Const_int i -> Lit_int (Int.to_string i)
  | Const_char c -> Lit_char (String.make 1 c)
  | Const_string (str, _, _) -> string str
  | Const_float f -> Lit_float f
